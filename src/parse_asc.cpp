#include <Rcpp.h>
#include <cctype>
#include <fstream>
#include <string>
#include <vector>
#include <cstdlib>

using namespace Rcpp;

static const size_t EST_SAMPLES = 1000000;

// Helper: Fast string-to-double handling EyeLink's missing '.'
inline double parse_eye_val(const char*& ptr) {
    while (*ptr == ' ' || *ptr == '\t') ptr++;
    if (*ptr == '.') {
        ptr++;
        while (*ptr != ' ' && *ptr != '\t' && *ptr != '\0') ptr++;
        return NA_REAL;
    }
    char* end;
    double val = std::strtod(ptr, &end);
    ptr = end;
    return val;
}

// Helper: Fast string-to-int
inline int parse_eye_int(const char*& ptr) {
    while (*ptr == ' ' || *ptr == '\t') ptr++;
    if (*ptr == '.') {
        ptr++;
        while (*ptr != ' ' && *ptr != '\t' && *ptr != '\0') ptr++;
        return NA_INTEGER;
    }
    char* end;
    long val = std::strtol(ptr, &end, 10);
    ptr = end;
    return static_cast<int>(val);
}

// [[Rcpp::export]]
List parse_asc_cpp(std::string path) {
    std::ifstream file(path);
    if (!file.is_open()) stop("Could not open the ASC file.");

    // --- 1. State Variables ---
    bool detected_eye = false;
    bool is_binocular = false;
    std::string monocular_eye = "L";

    // --- 2. Sample Vectors ---
    std::vector<int> s_time;
    std::vector<double> s_x, s_y, s_pupil;
    std::vector<std::string> s_eye;

    s_time.reserve(EST_SAMPLES); s_x.reserve(EST_SAMPLES);
    s_y.reserve(EST_SAMPLES); s_pupil.reserve(EST_SAMPLES); s_eye.reserve(EST_SAMPLES);

    // --- 3. Event Vectors ---
    std::vector<std::string> ev_type, ev_eye;
    std::vector<int> ev_st, ev_et, ev_dur;
    std::vector<double> ev_sx, ev_sy, ev_ex, ev_ey, ev_ax, ev_ay, ev_ap;

    int est_ev = 20000;
    ev_type.reserve(est_ev); ev_eye.reserve(est_ev); ev_st.reserve(est_ev);
    ev_et.reserve(est_ev); ev_dur.reserve(est_ev); ev_sx.reserve(est_ev);
    ev_sy.reserve(est_ev); ev_ex.reserve(est_ev); ev_ey.reserve(est_ev);
    ev_ax.reserve(est_ev); ev_ay.reserve(est_ev); ev_ap.reserve(est_ev);

    // --- 4. Metadata Vector (For R regex parsing) ---
    std::vector<std::string> meta_lines;
    meta_lines.reserve(10000);

    std::string line;

    // --- 5. Single-Pass Parsing Loop ---
    while (std::getline(file, line)) {
        if (line.empty()) continue;
        const char* ptr = line.c_str();

        // [A] SAMPLE LINES
        if (isdigit(ptr[0])) {
            // Verify it's a real sample (timestamp followed by space/tab/end)
            // to avoid misparsing log lines like "0: response(...)"
            const char* check_ptr = ptr;
            parse_eye_int(check_ptr);

            if (*check_ptr == ' ' || *check_ptr == '\t' || *check_ptr == '\0') {
                int t = parse_eye_int(ptr);
                double x1 = parse_eye_val(ptr);
                double y1 = parse_eye_val(ptr);
                double p1 = parse_eye_val(ptr);

                if (is_binocular) {
                    double x2 = parse_eye_val(ptr);
                    double y2 = parse_eye_val(ptr);
                    double p2 = parse_eye_val(ptr);

                    // Left Eye Row
                    s_time.push_back(t); s_x.push_back(x1); s_y.push_back(y1);
                    s_pupil.push_back(p1); s_eye.push_back("L");
                    // Right Eye Row
                    s_time.push_back(t); s_x.push_back(x2); s_y.push_back(y2);
                    s_pupil.push_back(p2); s_eye.push_back("R");
                } else {
                    s_time.push_back(t); s_x.push_back(x1); s_y.push_back(y1);
                    s_pupil.push_back(p1); s_eye.push_back(monocular_eye);
                }
            } else {
                // Not a sample, treat as metadata
                meta_lines.push_back(line);
            }
        }

        // [B] FIXATIONS
        else if (line.compare(0, 4, "EFIX") == 0) {
            ptr += 4; while (*ptr == ' ' || *ptr == '\t') ptr++;
            std::string eye(1, *ptr); ptr++;

            ev_type.push_back("FIXATION"); ev_eye.push_back(eye);
            ev_st.push_back(parse_eye_int(ptr)); ev_et.push_back(parse_eye_int(ptr));
            ev_dur.push_back(parse_eye_int(ptr));

            ev_sx.push_back(NA_REAL); ev_sy.push_back(NA_REAL);
            ev_ex.push_back(NA_REAL); ev_ey.push_back(NA_REAL);

            ev_ax.push_back(parse_eye_val(ptr)); ev_ay.push_back(parse_eye_val(ptr));
            ev_ap.push_back(parse_eye_val(ptr));
        }

        // [C] SACCADES
        else if (line.compare(0, 5, "ESACC") == 0) {
            ptr += 5; while (*ptr == ' ' || *ptr == '\t') ptr++;
            std::string eye(1, *ptr); ptr++;

            ev_type.push_back("SACCADE"); ev_eye.push_back(eye);
            ev_st.push_back(parse_eye_int(ptr)); ev_et.push_back(parse_eye_int(ptr));
            ev_dur.push_back(parse_eye_int(ptr));

            ev_sx.push_back(parse_eye_val(ptr)); ev_sy.push_back(parse_eye_val(ptr));
            ev_ex.push_back(parse_eye_val(ptr)); ev_ey.push_back(parse_eye_val(ptr));

            ev_ax.push_back(NA_REAL); ev_ay.push_back(NA_REAL); ev_ap.push_back(NA_REAL);
        }

        // [D] BLINKS
        else if (line.compare(0, 6, "EBLINK") == 0) {
            ptr += 6; while (*ptr == ' ' || *ptr == '\t') ptr++;
            std::string eye(1, *ptr); ptr++;

            ev_type.push_back("BLINK"); ev_eye.push_back(eye);
            ev_st.push_back(parse_eye_int(ptr)); ev_et.push_back(parse_eye_int(ptr));
            ev_dur.push_back(parse_eye_int(ptr));

            ev_sx.push_back(NA_REAL); ev_sy.push_back(NA_REAL);
            ev_ex.push_back(NA_REAL); ev_ey.push_back(NA_REAL);
            ev_ax.push_back(NA_REAL); ev_ay.push_back(NA_REAL); ev_ap.push_back(NA_REAL);
        }

        // [E] METADATA (MSG, START, END, etc.)
        else {
            meta_lines.push_back(line);

            // Detect binocular setup on the first START line
            if (!detected_eye && line.compare(0, 5, "START") == 0) {
                bool has_left = line.find("LEFT") != std::string::npos;
                bool has_right = line.find("RIGHT") != std::string::npos;
                if (has_left && has_right) is_binocular = true;
                else if (has_left) monocular_eye = "L";
                else if (has_right) monocular_eye = "R";
                detected_eye = true;
            }
        }
    }
    file.close();

    // --- 6. Rcpp DataFrame Construction ---
    DataFrame samples = DataFrame::create(
        Named("time") = wrap(s_time), Named("x") = wrap(s_x),
        Named("y") = wrap(s_y), Named("pupil") = wrap(s_pupil),
        Named("eye") = wrap(s_eye), Named("stringsAsFactors") = false
    );

    DataFrame events = DataFrame::create(
        Named("type") = wrap(ev_type), Named("eye") = wrap(ev_eye),
        Named("start_time") = wrap(ev_st), Named("end_time") = wrap(ev_et),
        Named("duration") = wrap(ev_dur), Named("x_start") = wrap(ev_sx),
        Named("y_start") = wrap(ev_sy), Named("x_end") = wrap(ev_ex),
        Named("y_end") = wrap(ev_ey), Named("avg_x") = wrap(ev_ax),
        Named("avg_y") = wrap(ev_ay), Named("avg_pupil") = wrap(ev_ap),
        Named("stringsAsFactors") = false
    );

    return List::create(
        Named("samples") = samples,
        Named("events") = events,
        Named("meta_lines") = wrap(meta_lines)
    );
}

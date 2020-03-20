#include "error.hpp"
#include <limits>

namespace ovid {

    static bool didError = false;

    std::nullptr_t logError(const std::string& msg, SourceLocation location) {
        didError = true;
        std::cout << "\x1b[1m" << location.filename << ":" << location.row << ":" << location.col << ": \x1b[31;1m" << msg << "\n\x1b[m";
        location.file.clear();
        if(location.file) {
            /* save location */
            auto oldLoc = location.file.tellg();
            /* seek to line row - 1 */
            location.file.seekg(std::ios::beg);
            for(int i = 0; i < location.row - 1; ++i){
                location.file.ignore(std::numeric_limits<std::streamsize>::max(),'\n');
            }

            std::string line;
            getline(location.file, line);
            std::cout << line << "\n";
            for(int i = 0; i < location.col - 1; i++) {
                if(isspace(line[i])) std::cout << line[i];
                else std::cout << ' ';
            }
            std::cout << "\x1b[1m^\x1b[m\n";

            location.file.seekg(oldLoc);

        } else {
            std::cout << "[ Can't print source location ]\n";
        }
        return nullptr;
    }

    bool errorOccurred() {
        return didError;
    }
}
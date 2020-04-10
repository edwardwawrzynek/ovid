#include "error.hpp"
#include <cstdio>
#include <limits>
#include <iomanip>

namespace ovid {

    static bool didError = false;

    enum ErrorPrintLevel {
        PRINT_ERROR, PRINT_WARNING
    };

    static ErrorPrintLevel errorTypeToPrintLevel(ErrorType type) {
        switch(type) {
            case PARSE_ERROR: return PRINT_ERROR;
            default: return PRINT_ERROR;
        }
    }

    std::nullptr_t logError(const std::string& msg, SourceLocation location, ErrorType type) {
        didError = true;
        auto printType = errorTypeToPrintLevel(type);
        std::cout << "\x1b[1m" << location.filename << ":" << location.row << ":" << location.col << ": ";
        if(printType == PRINT_ERROR)
            std::cout << "\x1b[1;31m";
        else if(printType == PRINT_WARNING)
            std::cout << "\x1b[1;33m";
        std::cout << msg << "\x1b[m\n";

        std::cout << "\x1b[1;34m" << std::setw(5) << location.row << " |\x1b[m ";
        location.file->clear();
        if(location.file) {
            /* save location */
            auto oldLoc = location.file->tellg();
            /* seek to line row - 1 */
            location.file->seekg(std::ios::beg);
            for(int i = 0; i < location.row - 1; ++i){
                location.file->ignore(std::numeric_limits<std::streamsize>::max(),'\n');
            }

            std::string line;
            getline(*location.file, line);
            std::cout << line << "\n\x1b[1;34m      | \x1b[m";
            for(int i = 0; i < location.col - 1; i++) {
                if(isspace(line[i])) std::cout << line[i];
                else std::cout << ' ';
            }
            if(printType == PRINT_ERROR)
                std::cout << "\x1b[31;1m^\x1b[m\n";
            else if(printType == PRINT_WARNING)
                std::cout << "\x1b[33;1m^\x1b[m\n";

            location.file->seekg(oldLoc);

        } else {
            std::cout << "[ Can't print source location ]\n";
        }
        return nullptr;
    }

    bool errorOccurred() {
        return didError;
    }
}
#include <SFML/Graphics.hpp>
#include <iostream>
#include <vector>
#include <string>
#include <stack>
#include <regex>
#include <optional>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <cmath>
#include <array>
#include <chrono>
#include <numbers>

class ReversePolishNotation {
private:
    std::unordered_map<std::string, int> operations{ {"(", 1}, {")", 1}, {"+", 2}, {"-", 2}, {"*", 3}, {"/", 3}, {"^", 4}, {"~", 5}, {"∔", 5} };
    std::unordered_set<std::string> functions{ "sin", "cos", "ln" };
private:
    inline int GetOpPriority(const std::string& c) const {
        if (operations.find(c) != operations.end()) return (*(operations.find(c))).second;
        return -1;
    }
    inline bool IsNumber(const std::string& s) const {
        std::regex numberRegex("^[-+]?([0-9]*\\.[0-9]+|[0-9]+)$");
        return std::regex_match(s, numberRegex) || s == "e" || s == "pi";
    }
    inline bool IsFunction(const std::string& s) const {
        return (functions.find(s) != functions.end());
    }
    inline bool IsOperator(const std::string& s) const {
        return (operations.find(s) != operations.end());
    }

private:
    [[nodiscard]] std::vector<std::string> SplitExpression(const std::string& expression) const {
        std::vector<std::string> result;
        for (std::size_t i = 0; i < expression.size(); ++i) {
            if (expression[i] == ' ') continue;
            if (operations.find({ expression[i] }) != operations.end()) {
                result.push_back({ expression[i] });
            }
            else if (expression.substr(i, 3) == "cos" || expression.substr(i, 3) == "sin") {
                result.push_back(expression.substr(i, 3));
                i += 2;
            }
            else if (expression.substr(i, 2) == "ln") {
                result.push_back(expression.substr(i, 2));
                i += 1;
            }
            else if (expression.substr(i, 2) == "pi") {
                result.push_back(expression.substr(i, 2));
                i += 1;
            }
            else if (expression[i] == 'e' || expression[i] == 'x') {
                result.push_back({ expression[i] });
            }
            else {
                std::string current_string;
                while (std::isdigit(expression[i]) || expression[i] == '.') {
                    current_string.push_back(expression[i++]);
                }
                --i;
                if (IsNumber(current_string)) result.push_back(current_string);
                else {
                    std::cout << "Sorry Wrong number format\n";
                    return {};
                }
            }
        }
        return result;
    }
public:
    std::vector<std::string> TransformFromInfix(const std::string& infix_expression) const {
        std::stack<std::string> operators;
        std::vector<std::string> postfix; postfix.reserve(infix_expression.size());

        auto elements = SplitExpression(infix_expression);
        for (std::size_t i = 0; i < elements.size(); ++i) {
            auto element = elements[i];
            if (IsNumber(element) || element == "x") {
                postfix.push_back(element);
                if (i != elements.size() - 1 && elements[i + 1] == "(") element = "*";
            }
            if (element == "(") {
                operators.push(element);
            }
            else if (element == ")") {
                while (operators.top() != "(") {
                    postfix.push_back({ operators.top() });
                    operators.pop();

                    if (operators.empty()) throw "Wrong operator or operand format!";
                }
                operators.pop();
                if (!operators.empty() && this->IsFunction(operators.top())) { // Обработка функций
                    postfix.push_back(operators.top());
                    operators.pop();
                }
            }
            else if (this->IsOperator(element)) {
                if (i == elements.size() - 1 || (i != 0 && IsOperator(elements[i - 1])
                    && elements[i - 1] != ")" && elements[i - 1] != "(")) { // Два оператора подряд или оператор в конце
                    std::cout << "Wrong Operator or Operand format\n";
                    return {};
                }
                if (element == "-" && ((i == 0 || elements[i - 1] == "("))) {
                    operators.push({ "~" });
                    continue;
                }
                if (element == "+" && ((i == 0 || elements[i - 1] == "("))) {
                    operators.push({ "∔" });
                    continue;
                }
                int curr_prior = GetOpPriority(element);
                while (!operators.empty() && GetOpPriority(operators.top()) >= curr_prior && operators.top() != "^") {
                    postfix.push_back({ operators.top() });
                    operators.pop();
                }
                operators.push(element);
            }
            else if (this->IsFunction(element)) {
                operators.push(element);
            }

            // Если текущий элемент не оператор и следующий за ним тоже не оператор, то перемножаем их
            if (i != elements.size() - 1 && this->IsFunction(element) == false) {
                if ((this->IsOperator(element) == false || element == ")") && (this->IsOperator(elements[i + 1]) == false || elements[i + 1] == "(")) {
                    operators.push("*");
                }
            }
        }
        while (!operators.empty()) {
            postfix.push_back({ operators.top() });
            operators.pop();
        }

        return postfix;
    }

    std::optional<long double> CalculateRPN(const std::vector<std::string>& postfix_expression, bool need_warning = true) const {
        if (postfix_expression.size() == 1) {
            if (IsNumber(postfix_expression[0])) return std::make_optional(std::stod(postfix_expression[0]));
            return std::nullopt;
        }
        long double f1 = 0, f2 = 0;
        std::stack<long double> stack;
        for (const auto& el : postfix_expression) {
            try {
                if (this->IsNumber(el)) {
                    if (el == "e") stack.push(std::numbers::e);
                    else if (el == "pi") stack.push(std::numbers::pi);
                    else stack.push(std::stod(el));
                    continue;
                }
                if (this->IsFunction(el)) {
                    auto f = stack.top(); stack.pop();
                    if (el == "cos") stack.push(cos(f));
                    else if (el == "sin") stack.push(sin(f));
                    else if (el == "ln") stack.push(log(f));
                    continue;
                }
                if (el == "∔") continue;
                if (el == "~") {
                    auto f = stack.top(); stack.pop();
                    stack.push(-f);
                    continue;
                }

                f1 = stack.top(); stack.pop();
                f2 = stack.top(); stack.pop();

                if (el == "+") stack.push(f2 + f1);
                else if (el == "-") stack.push(f2 - f1);
                else if (el == "*") stack.push(f2 * f1);
                else if (el == "/") {
                    if (f1 == 0) {
                        if (need_warning) std::cout << "Division by zero(\n";
                        return std::nullopt;
                    }
                    stack.push(f2 / (double)f1);
                }
                else if (el == "^") stack.push(std::pow(f2, f1));
            }
            catch (...) {
                if (need_warning) std::cout << "Wrong Operator or Operand format\n";
                return std::nullopt;
            }
        }
        return (stack.empty() ? std::nullopt : std::make_optional(stack.top()));
    }

    void EvaluateRPN(const std::string& expression) const {
        if (expression.empty()) {
            std::cout << "Empty Expression!\n";
            return;
        }
        std::vector<std::string> rpn = TransformFromInfix(expression);
        std::cout << "Reverse Polish Notation : ";
        for (const auto& el : rpn) std::cout << el << " ";
        std::cout << "\n";
        if (expression.find('x') != std::string::npos) return;
        auto result = CalculateRPN(rpn);
        if (result != std::nullopt) std::cout << "Result : " << *CalculateRPN(rpn) << "\n";
    }
};

class Differentiation {
private:
    std::unordered_set<char> operations{ '(', ')', '+', '-', '*', '/', '^', '~' };
    std::unordered_set<std::string> functions{ "sin", "cos", "ln" };
private:
    inline bool IsFunction(const std::string& s) const {
        return (functions.find(s) != functions.end());
    }
    inline bool IsOperator(const char& s) const {
        return (operations.find(s) != operations.end());
    }
    inline bool ContainsX(const std::string& expression) {
        return expression.find('x') != std::string::npos;
    }
    std::optional<std::string> GetPureString(const std::string& string) {
        // Проставить где надо умножение, проверить все последовательность на корректность скобок, убрать пробелы, проверить саму последовательность(символы)
        return std::make_optional(string);
    }
private:
    std::vector<std::string> PerformSplitPlusMinus(const std::string& expression) {
        std::vector<std::string> result;
        std::string current_block; current_block.reserve(expression.size());
        for (std::size_t i = 0, open_braces_count = 0; i < expression.size(); ++i) {
            if (expression[i] == '+' || expression[i] == '-') {
                if (open_braces_count == 0) {
                    result.push_back(current_block);
                    current_block.clear();
                    if (expression[i] == '-') current_block.push_back('-'); // Сохраняем унарные минусы
                }
                else {
                    current_block.push_back(expression[i]);
                }
                continue;
            }
            if (expression[i] == '(') ++open_braces_count;
            else if (expression[i] == ')') --open_braces_count;
            current_block.push_back(expression[i]);
        }
        if (!expression.empty() && expression.back() != '+' && expression.back() != '-') result.push_back(current_block);
        return result;
    }
    std::optional<std::pair<std::string, std::string>> PerformSplitMult(const std::string& expression) {
        std::size_t index_of_mult = std::string::npos;
        for (std::size_t i = 0, open_braces_count = 0; i < expression.size(); ++i) {
            if (expression[i] == '*' && open_braces_count == 0) {
                index_of_mult = i;
                break;
            }
            if (expression[i] == '(') ++open_braces_count;
            else if (expression[i] == ')') --open_braces_count;
        }

        if (index_of_mult == std::string::npos) return std::nullopt;
        return std::make_optional(
            std::make_pair(std::string(expression.begin(), expression.begin() + index_of_mult),
                std::string(expression.begin() + index_of_mult + 1, expression.end()))
        );
    }
    std::optional<std::pair<std::pair<std::string, std::string>, std::string>> PerformSplitDiv(const std::string& expression) {
        // Return numerator, denominator and next_part
        std::size_t index_of_div = std::string::npos, index_of_next_sign = expression.size();
        for (std::size_t i = 0, open_braces_count = 0; i < expression.size(); ++i) {
            if (open_braces_count == 0) {
                if (expression[i] == '/' && index_of_div == std::string::npos) index_of_div = i;
                else if (index_of_div != std::string::npos && IsOperator(expression[i])) {
                    index_of_next_sign = i;
                    break;
                }
            }
            if (expression[i] == '(') ++open_braces_count;
            else if (expression[i] == ')') --open_braces_count;
        }

        if (index_of_div == std::string::npos) return std::nullopt;
        return std::make_optional(std::make_pair(
            std::make_pair(std::string(expression.begin(), expression.begin() + index_of_div),
                std::string(expression.begin() + index_of_div + 1, expression.begin() + index_of_next_sign)),
            std::string(expression.begin() + index_of_next_sign, expression.end())));
    }
    std::optional<std::pair<std::string, std::string>> PerformSplitExp(const std::string& expression) {
        std::size_t index_of_exp = std::string::npos;
        for (std::size_t i = 0, open_braces_count = 0; i < expression.size(); ++i) {
            if (expression[i] == '^' && open_braces_count == 0) {
                index_of_exp = i;
                break;
            }
            if (expression[i] == '(') ++open_braces_count;
            else if (expression[i] == ')') --open_braces_count;
        }

        if (index_of_exp == std::string::npos) return std::nullopt;
        return std::make_optional(
            std::make_pair(std::string(expression.begin(), expression.begin() + index_of_exp),
                std::string(expression.begin() + index_of_exp + 1, expression.end())));
    }
private:
    std::string CombinePlusMinusState(const std::vector<std::string>&& expression) {
        std::string outcome;
        for (const auto& sub_expr : expression) {
            if (sub_expr.empty()) continue;
            if (outcome.empty()) {
                outcome += sub_expr;
                continue;
            }
            outcome += (sub_expr[0] == '-' ? sub_expr : ("+" + sub_expr));
        }
        return outcome;
    }
    std::string ExecuteMultiply(const std::vector<std::string>& left, const std::vector<std::string>& right) {
        std::string outcome;
        for (const auto& g1 : left) {
            for (const auto& g2 : right) {
                if (!outcome.empty()) outcome += "+";
                if (g1 != "1" && g2 != "1") outcome += (g1 + "*" + g2);
                else if (g1 == "1") outcome += g2;
                else outcome += g1;
            }
        }
        //std::cout << outcome << "\n";
        return outcome;
    }

    std::vector<std::string> GetNegative(const std::vector<std::string>& expr) {
        auto t = expr;
        for (auto& el : t) {
            if (el.empty()) continue;
            if (el[0] == '-') el.erase(el.begin());
            else el.insert(el.begin(), '-');
        }
        return t;
    }
    std::vector<std::string> GetNegative(const std::string& expr) {
        return GetNegative(PerformSplitPlusMinus(expr));
    }

    std::string GetSum(const std::string& el1, const std::string& el2) {
        std::vector<std::string> result,
            sub_expr1 = PerformSplitPlusMinus(el1),
            sub_expr2 = PerformSplitPlusMinus(el2);
        for (auto& sub_expr : sub_expr1) result.push_back(std::move(sub_expr));
        for (auto& sub_expr : sub_expr2) result.push_back(std::move(sub_expr));
        return CombinePlusMinusState(std::move(result));
    }
    std::string GetDifference(const std::string& el1, const std::string& el2) {
        return GetSum(el1, CombinePlusMinusState(GetNegative(el2)));
    }

private:
    std::string DifferentiateExpression(const std::string& executed_function) {
        std::string function;
        if (executed_function.empty()) return {};
        if (executed_function.front() == '(' && executed_function.back() == ')'
            && std::count(executed_function.begin(), executed_function.end(), '(') == 1) function = std::string(executed_function.begin() + 1, executed_function.end() - 1);
        else function = executed_function;
        //std::cout << function << "\n";
        {
            auto plus_minus_split = PerformSplitPlusMinus(function);
            if (plus_minus_split.size() > 1) { // Если удалось просплитовать по знакам плюс/минус
                std::vector<std::string> cur_result;
                for (const auto& sub_expr : plus_minus_split) {
                    cur_result.push_back(DifferentiateExpression(sub_expr));
                }
                return CombinePlusMinusState(std::move(cur_result));
            }
        }

        {
            auto mul_split = PerformSplitMult(function);
            if (mul_split != std::nullopt) {
                auto left_derivative = PerformSplitPlusMinus(DifferentiateExpression((*mul_split).first));
                auto right_derivative = PerformSplitPlusMinus(DifferentiateExpression((*mul_split).second));

                return GetSum(ExecuteMultiply(left_derivative, PerformSplitPlusMinus((*mul_split).second)),
                    ExecuteMultiply(PerformSplitPlusMinus((*mul_split).first), right_derivative));
            }
        }

        {
            auto div_split = PerformSplitDiv(function);
            if (div_split != std::nullopt) {
                if (ContainsX((*div_split).first.first) == false && ContainsX((*div_split).first.second) == false)
                    return {};
                auto numerator_derivative = PerformSplitPlusMinus(DifferentiateExpression((*div_split).first.first));
                auto denominator_derivative = PerformSplitPlusMinus(DifferentiateExpression((*div_split).first.second));

                if ((*div_split).first.second.empty() || (*div_split).first.second == "0") {
                    std::cout << "Division by zero / Empty expression\n";
                    return {};
                }

                std::string res = "(" + GetDifference(ExecuteMultiply(numerator_derivative, PerformSplitPlusMinus((*div_split).first.second)),
                    ExecuteMultiply(PerformSplitPlusMinus((*div_split).first.first), denominator_derivative)) + ")"
                    "/" + "(" + (*div_split).first.second + ")" + "^" + "2";

                // Execute of differentiation of next_part
                return ((*div_split).second.empty() == false ? // There is a next_part?
                    DifferentiateExpression(res + (*div_split).second) : res);
            }
        }

        {
            auto exp_split = PerformSplitExp(function);
            if (exp_split != std::nullopt) {
                //std::cout << "Splited " << function << " to " << (*exp_split).first << " " << (*exp_split).second << "\n";
                auto first = (*exp_split).first;
                auto second = (*exp_split).second;
                if (ContainsX(second)) {
                    if (ContainsX(first)) {
                        second += "*ln(" + first + ")";
                        first = "e";
                    }
                    std::string sub_expr_der = DifferentiateExpression(second);
                    return (sub_expr_der.empty() == false ? "(" + sub_expr_der + ")" + "*" : "") + function + "*ln(" + first + ")";
                }
                else if (ContainsX(first)) {
                    std::string sub_expr_der = DifferentiateExpression(first);
                    return (sub_expr_der.empty() == false ? sub_expr_der + "*" : "") +
                        second + "*(" + first + ")^(" + second + "-1)";
                }
                return ""; //Число в степени числа
            }
        }

        {
            if (function.size() > 3 && function.substr(0, 3) == "cos") {
                std::size_t start = 3, end = function.size();
                if (function[start] == '(') ++start;
                if (function[end - 1] == ')') --end;
                std::string sub_expr = function.substr(start, end - start);
                std::string sub_expr_der = DifferentiateExpression(sub_expr);
                return (sub_expr_der.empty() ? "sin(" + sub_expr + ")" : "(" +
                    CombinePlusMinusState(GetNegative(sub_expr_der)) + ")" + "*sin(" + sub_expr + ")");
            }
        }

        {
            if (function.size() > 3 && function.substr(0, 3) == "sin") {
                std::size_t start = 3, end = function.size();
                if (function[start] == '(') ++start;
                if (function[end - 1] == ')') --end;
                std::string sub_expr = function.substr(start, end - start);
                std::string sub_expr_der = DifferentiateExpression(sub_expr);
                return (sub_expr_der.empty() ? "cos(" + sub_expr + ")" : sub_expr_der + "*cos(" + sub_expr + ")");
            }
        }

        {
            if (function.size() > 3 && function.substr(0, 2) == "ln") {
                std::size_t start = 2, end = function.size();
                if (function[start] == '(') ++start;
                if (function[end - 1] == ')') --end;
                std::string sub_expr = function.substr(start, end - start);
                std::string sub_expr_der = DifferentiateExpression(sub_expr);
                return (sub_expr_der.empty() ? ("*1/(" + sub_expr + ")") : (sub_expr_der + "*1/(" + sub_expr + ")"));
            }
        }

        return (ContainsX(executed_function) ? "1" : "");
    }
public:
    void DifferentiateFunction(const std::string& expression, long double x0_ = UINT32_MAX) {
        auto replace_x = [](const std::vector<std::string>& expr, long double x) -> std::vector<std::string> {
            std::vector<std::string> res; res.reserve(expr.size());
            for (auto& el : expr) res.push_back(el == "x" ? std::to_string(x) : el);
            return res;
            };
        
        auto pure_string = GetPureString(expression);
        if (pure_string == std::nullopt) {
            std::cout << "Your expression contains an error(\n";
            return;
        }
        auto derived = DifferentiateExpression(*pure_string);
        if (derived.empty() == false) std::cout << "The derivative of the function y(x) = " << *pure_string << " is " << "y'(x) = " << derived << "\n";
        if (x0_ == UINT32_MAX) return;

        static ReversePolishNotation rpn;
        auto postfix_func = rpn.TransformFromInfix(expression);
        auto postfix_func_der = rpn.TransformFromInfix(derived);

        auto fx0 = rpn.CalculateRPN(replace_x(postfix_func, x0_)),
            fdx0 = rpn.CalculateRPN(replace_x(postfix_func_der, x0_));
        if (fx0 == std::nullopt) {
            std::cout << "It was not possible to calculate the value of the function at the point x0, check the expression\n";
            return;
        }
        if (fdx0 == std::nullopt) {
            std::cout << "It was not possible to calculate the value of the derivative at the point x0, check the expression\n";
            return;
        }
        std::cout << "The value of the function at the point x0 = " << x0_ << " : y(x0) = " << *fx0 << "\n";
        std::cout << "The value of the derivative of the function at the point x0 = " << x0_ << " : y'(x0) = " << *fdx0 << "\n";
        std::cout << "The equation of the tangent to the graph of the function u(x) = " << *fdx0 << "(x - " << x0_ << ") + " << *fx0 << "\n";

        // Построение графика и касательной к функции

        static const std::size_t H = 800, W = 800;
        sf::RenderWindow window(sf::VideoMode(H, W), "Function Grapich!");
        sf::CircleShape point1(2.f);
        sf::CircleShape point2(1.f);

        static const float c = 30;
        static const long x0 = 400, y0 = 400, mass = 10000, scale = 30;

        sf::RectangleShape line[40];
        for (int i = 0; i < 40; i++) {
            line[i].setSize(sf::Vector2f(1, 20));
            line[i].setFillColor(sf::Color::Black);

            if (i < 20) {
                if (i < 10) line[i].setPosition(x0 - (i + 1) * scale, y0 - 10);
                else line[i].setPosition(x0 + (i - 9) * scale, y0 - 10);
            }
            else {
                line[i].setRotation(90);
                if (i < 30) line[i].setPosition(x0 + 10, y0 + (i - 30) * scale);
                else line[i].setPosition(x0 + 10, y0 + (i - 29) * scale);
            }
        }

        sf::RectangleShape OsX(sf::Vector2f(W, 1));
        OsX.setFillColor(sf::Color::Black);
        OsX.setPosition(0, y0);

        sf::RectangleShape OsY(sf::Vector2f(1, H));
        OsY.setFillColor(sf::Color::Black);
        OsY.setPosition(x0, 0);

        sf::RectangleShape strel[4];
        for (int i = 0; i < 4; i++) {
            strel[i].setSize(sf::Vector2f(1, 25));
            strel[i].setFillColor(sf::Color::Black);

            if (i < 2) strel[i].setPosition(x0, 0);
            else strel[i].setPosition(W, y0);
        }
        strel[0].setRotation(25);
        strel[1].setRotation(-25);
        strel[2].setRotation(60);
        strel[3].setRotation(-250);

        std::array<std::optional<long double>, 2 * mass> v;
        for (long i = -mass; i < mass; ++i) {
            v[mass + i] = rpn.CalculateRPN(replace_x(postfix_func, float(i) / 10), false);
            //std::cout << i << " " << *v.back() << "\n";
        }

        while (window.isOpen()) {
            sf::Event event;
            while (window.pollEvent(event)) {
                if (event.type == sf::Event::Closed) window.close();
            }

            window.clear(sf::Color::White);
            window.draw(OsX);
            window.draw(OsY);

            for (int i = 0; i < 4; i++) window.draw(strel[i]);
            for (int i = 0; i < 40; i++)
                if (i != 19 && i != 20) window.draw(line[i]);

            point1.setFillColor(sf::Color::Red);
            for (long i = -mass; i < mass; ++i) {
                try {
                    float x = i;
                    auto y = v[mass + i]; long double h = i;
                    while (y == std::nullopt) {
                        y = v[mass + h];
                        --h;
                    }
                    float x1 = x0 + (x / 10) * scale;
                    float y1 = y0 - (*y) * scale;
                    point1.setPosition(x1, y1);
                    window.draw(point1);
                }
                catch (...) {}
            }

            point2.setFillColor(sf::Color::Blue);
            for (long double i = -mass; i < mass; i += 0.1) {
                try {
                    float x = i / c;
                    auto y = (*fdx0) * (x - x0_) + (*fx0);
                    float x1 = x0 + x * scale;
                    float y1 = y0 - y * scale;
                    point2.setPosition(x1, y1);
                    window.draw(point2);
                }
                catch (...) {}
            }

            window.display();
        }

    }
};

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(0);
    std::cin.tie(0);

    const auto start{ std::chrono::steady_clock::now() };

    std::cout << "This program allows you to calculate at each iteration:\n1. Calculation of the expression through the reverse Polish notation\n2. Calculate the derivative of the function and plot the tangent to the graph\n";
    std::string s;
    while (true) {
        int n = 0, x = 0; std::cin >> n;
        switch (n) {
        case 1:
            std::cin >> s;
            ReversePolishNotation().EvaluateRPN(s);
            break;
        case 2:
            std::cin >> s >> x;
            Differentiation().DifferentiateFunction(s, x);
            break;
        }
    }
    return EXIT_SUCCESS;
}

#include <algorithm>
#include <iostream>
#include <vector>
#include <string>
#include <stdexcept>
#include <cmath>
#include <random>

#define ll long long
using namespace std;

class TLong {
private:
    void _normalize() {
        while (digits.size() > 1 && digits.back() == '0') {
            digits.pop_back();
        }
    }
    static TLong _mod_exp(TLong base, TLong exp, const TLong& mod) {
        TLong result(1);
        base = base % mod;

        while (exp > 0) {
            if (exp % 2 == 1) {
                result = (result * base) % mod;
            }
            exp = exp / 2;
            base = (base * base) % mod;
        }
        return result;
    }
public:
    vector<char> digits;
    bool is_neg = false;

    explicit TLong(string num = "") {
        if (num.empty()) { return; }
        if (num[0] == '-') {
            is_neg = true;
            num.erase(0, 1);
        }

        for (size_t i = num.size(); i-- > 0;) {
            if (isdigit(num[i])) {
                digits.push_back(num[i]);
            }
            else {
                throw invalid_argument("Invalid character in number string");
            }
        }
    }
    explicit TLong(ll num) {
        if (num < 0) {
            is_neg = true;
            num *= -1;
        }
        if (num == 0) {
            digits.push_back('0');
        }
        else {
            while (num > 0) {
                digits.push_back(static_cast<char>('0' + (num % 10)));
                num /= 10;
            }
        }
    }

    TLong operator + (const TLong& other) const {
        if (!(is_neg || other.is_neg)) { // both > 0
            TLong res;
            res.digits.clear();
            int carry = 0;

            const size_t max_len = max(digits.size(), other.digits.size());

            for (size_t i = 0; i < max_len || carry; ++i) {
                int digitSum = carry;
                if (i < digits.size()) digitSum += digits[i] - '0';
                if (i < other.digits.size()) digitSum += other.digits[i] - '0';

                res.digits.push_back(static_cast<char>('0' + (digitSum % 10)));
                carry = digitSum / 10;
            }
            return res;

        }
        else if (is_neg && other.is_neg) { // both < 0
            TLong this_copy = *this;
            TLong other_copy = other;
            this_copy.is_neg = false;
            other_copy.is_neg = false;
            return -(this_copy + other_copy);

        }
        else if (is_neg && !other.is_neg) { // this < 0, other > 0
            TLong this_copy = *this;
            this_copy.is_neg = false;
            return other - this_copy;

        }
        else { // this > 0, other < 0
            TLong other_copy = other;
            other_copy.is_neg = false;
            TLong this_copy = *this;
            return this_copy - other_copy;
        }
    }
    TLong operator - (const TLong& other) const {
        if (!is_neg && !other.is_neg) { // both > 0
            if (*this >= other) {
                TLong res;
                res.digits.clear();
                int borrow = 0;

                const size_t max_len = max(digits.size(), other.digits.size());
                for (size_t i = 0; i < max_len; ++i) {
                    int digitA = (i < digits.size()) ? digits[i] - '0' : 0;
                    int digitB = (i < other.digits.size()) ? other.digits[i] - '0' : 0;

                    digitA -= borrow;

                    if (digitA < digitB) {
                        digitA += 10;
                        borrow = 1;
                    }
                    else { borrow = 0; }
                    res.digits.push_back(static_cast<char>((digitA - digitB) + '0'));
                }
                res._normalize();
                return res;
            }
            else {
                TLong result = other - *this;
                result.is_neg = true;
                return result;
            }

        }
        else if (is_neg && other.is_neg) { // both < 0
            TLong this_copy = *this;
            TLong other_copy = other;
            this_copy.is_neg = false;
            other_copy.is_neg = false;
            return other_copy - this_copy;

        }
        else if (is_neg && !other.is_neg) { // this < 0, other > 0
            TLong this_copy = *this;
            this_copy.is_neg = false;
            return -(other + this_copy);

        }
        else { // this > 0, other < 0
            TLong other_copy = other;
            other_copy.is_neg = false;
            return *this + other_copy;
        }
    }
    TLong operator * (const TLong& other) const {
        TLong result;
        if (is_neg != other.is_neg) {
            result.is_neg = true;
        }

        result.digits.resize(digits.size() + other.digits.size(), '0');

        for (size_t i = 0; i < digits.size(); ++i) {
            int carry = 0;
            int digitA = digits[i] - '0';

            for (size_t j = 0; j < other.digits.size(); ++j) {
                int digitB = other.digits[j] - '0';
                int product = digitA * digitB + carry + (result.digits[i + j] - '0');

                result.digits[i + j] = static_cast<char>((product % 10) + '0');
                carry = product / 10;
            }

            if (carry > 0) {
                result.digits[i + other.digits.size()] = static_cast<char>(result.digits[i + other.digits.size()] + carry);
            }
        }
        result._normalize();
        return result;
    }
    TLong operator / (const TLong& other) const {
        if (other == 0) {
            throw invalid_argument("Division by zero");
        }

        TLong numerator = *this;
        TLong divisor = other;
        numerator.is_neg = false;
        divisor.is_neg = false;

        TLong result;
        result.is_neg = (is_neg != other.is_neg);

        TLong remainder;
        for (size_t i = numerator.digits.size(); i-- > 0;) {
            remainder.digits.insert(remainder.digits.begin(), numerator.digits[i]);
            remainder._normalize();

            int quotient_digit = 0;
            while (remainder >= divisor) {
                remainder = remainder - divisor;
                quotient_digit++;
            }

            result.digits.insert(result.digits.begin(), static_cast<char>(quotient_digit + '0'));
        }

        result._normalize();
        return result;
    }
    TLong operator % (const TLong& other) const {
        if (other == TLong("0")) {
            throw invalid_argument("Division by zero is not allowed");
        }

        TLong this_abs = *this;
        TLong other_abs = other;

        this_abs.is_neg = false;
        other_abs.is_neg = false;

        const TLong div = this_abs / other_abs;
        const TLong product = div * other_abs;
        TLong result = this_abs - product;

        if (is_neg) { result.is_neg = true; }
        return result;
    }

    TLong operator + (const int num) const {
        return *this + TLong(num);
    }
    TLong operator - (const int num) const {
        return *this - TLong(num);
    }
    TLong operator * (const int num) const {
        return *this * TLong(num);
    }
    TLong operator / (const int num) const {
        if (num == 0) {
            throw invalid_argument("Division by zero is not allowed");
        }
        return *this / TLong(num);
    }
    TLong operator % (const int num) const {
        if (num == 0) {
            throw invalid_argument("Division by zero is not allowed");
        }
        return *this % TLong(num);
    }
    TLong operator - () const {
        TLong res = *this;
        res.is_neg = !is_neg;
        return res;
    }

    friend ostream& operator << (ostream& os, const TLong& num) {
        if (num.is_neg) { os << '-'; }

        for (size_t i = num.digits.size(); i-- > 0;) {
            os << num.digits[i];
        }
        return os;
    }
    bool operator >= (const TLong& other) const {
        if (is_neg != other.is_neg) { return !is_neg; }
        if (is_neg) {
            if (digits.size() != other.digits.size()) {
                return digits.size() < other.digits.size();
            }
            for (size_t i = digits.size(); i-- > 0;) {
                if (digits[i] != other.digits[i]) {
                    return digits[i] < other.digits[i];
                }
            }
        }
        else {
            if (digits.size() != other.digits.size()) {
                return digits.size() > other.digits.size();
            }
            for (size_t i = digits.size(); i-- > 0;) {
                if (digits[i] != other.digits[i]) {
                    return digits[i] > other.digits[i];
                }
            }
        }
        return true;
    }
    bool operator > (const TLong& other) const {
        if (is_neg != other.is_neg) { return !is_neg; }

        if (is_neg) {
            if (digits.size() != other.digits.size()) {
                return digits.size() < other.digits.size();
            }
            for (size_t i = digits.size(); i-- > 0;) {
                if (digits[i] != other.digits[i]) {
                    return digits[i] < other.digits[i];
                }
            }
        }
        else {
            if (digits.size() != other.digits.size()) {
                return digits.size() > other.digits.size();
            }
            for (size_t i = digits.size(); i-- > 0;) {
                if (digits[i] != other.digits[i]) {
                    return digits[i] > other.digits[i];
                }
            }
        }
        return false;
    }
    bool operator == (const TLong& other) const {
        if (is_neg != other.is_neg) { return false; }
        if (digits.size() != other.digits.size()) { return false; }
        for (size_t i = 0; i < digits.size(); ++i) {
            if (digits[i] != other.digits[i]) { return false; }
        }
        return true;
    }
    bool operator <= (const TLong& other) const {
        return !(*this > other);
    }
    bool operator <  (const TLong& other) const {
        return !(*this >= other);
    }
    bool operator != (const TLong& other) const {
        return !(*this == other);
    }

    bool operator >= (const int num) const {
        return *this >= TLong(num);
    }
    bool operator >  (const int num) const {
        return *this > TLong(num);
    }
    bool operator <= (const int num) const {
        return *this <= TLong(num);
    }
    bool operator <  (const int num) const {
        return *this < TLong(num);
    }
    bool operator == (const int num) const {
        return *this == TLong(num);
    }
    bool operator != (const int num) const {
        return *this != TLong(num);
    }

    [[nodiscard]] ll to_int() const {
        const short sign = (is_neg) ? -1 : 1;
        ll result = 0;
        for (size_t i = digits.size(); i-- > 0;) {
            const int digit = digits[i] - '0';
            if (result > (INT_MAX - digit) / 10) {
                throw std::overflow_error("Longint value is too large for int");
            }
            result = result * 10 + digit;
        }
        return result * sign;
    };
};
class TRational {
private:
    static void _check_rational(const TLong& numerator, const TLong& denominator) {
        if (denominator == 0) {
            throw invalid_argument("Denominator is 0!");
        }
    }
    static TLong _gcd(const TLong& a, const TLong& b) {
        const TLong zero = TLong("0");
        TLong tempA = a, tempB = b;

        while (tempB != zero) {
            const TLong temp = tempB;
            tempB = tempA % tempB;
            tempA = temp;
        }
        return tempA;
    }
    void _reduce_fraction() {
        const TLong gcd = _gcd(num, den);
        num = num / gcd;
        den = den / gcd;

        if (num == 0) {
            den = TLong(1);
        }
    }

public:
    TLong num = TLong(1), den = TLong(1);
    bool is_neg = false;

    TRational() = default;
    explicit TRational(TLong numerator, TLong denominator) {
        if ((numerator.is_neg && !denominator.is_neg) || (!numerator.is_neg && denominator.is_neg)) {
            is_neg = true;
        }
        if (numerator.is_neg) { numerator = -numerator; }
        if (denominator.is_neg) { denominator = -denominator; }

        _check_rational(numerator, denominator);
        num = numerator;
        den = denominator;
        _reduce_fraction();
    }
    explicit TRational(ll numerator, ll denominator) {
        if ((numerator < 0 && denominator > 0) || (numerator > 0 && denominator < 0)) {
            is_neg = true;
        }
        if (numerator < 0) { numerator = -numerator; }
        if (denominator < 0) { denominator = -denominator; }

        const TLong Tnum = TLong(numerator), Tden = TLong(denominator);
        _check_rational(Tnum, Tden);
        num = Tnum;
        den = Tden;
        _reduce_fraction();
    }
    explicit TRational(double number) {
        if (number < 0) {
            is_neg = true;
            number = -number;
        }

        constexpr ll max_decimals = 10000000;

        den = TLong(max_decimals);
        num = TLong(static_cast<ll>(number * max_decimals));
        _reduce_fraction();
    }

    TRational operator + (const TRational& other) const {
        const TLong common_den = den * other.den;
        const TLong result_num = num * other.den + den * other.num;
        if (is_neg && other.is_neg) { // both < 0
            return TRational(-result_num, common_den);
        }
        else if (is_neg) { // this < 0, other > 0
            return TRational(-num * other.den + den * other.num, common_den);
        }
        else if (other.is_neg) { // this > 0, other < 0
            return TRational(num * other.den - den * other.num, common_den);
        }
        else { // both > 0
            return TRational(result_num, common_den);
        }
    }
    TRational operator - (const TRational& other) const {
        if ((num == other.num) && (den == other.den)) {
            return TRational(0);
        }
        const TLong common_den = den * other.den;
        const TLong result_num = num * other.den - den * other.num;
        if (is_neg && other.is_neg) { // both < 0
            return TRational(-result_num, common_den);
        }
        else if (is_neg) { // this < 0, other > 0
            return TRational(-num * other.den - den * other.num, common_den);
        }
        else if (other.is_neg) { // this > 0, ther < 0
            return TRational(num * other.den + den * other.num, common_den);
        }
        else { // both > 0
            return TRational(result_num, common_den);
        }
    }
    TRational operator * (const TRational& other) const {
        const int sign = (is_neg != other.is_neg) ? -1 : 1;
        return TRational(num * other.num * sign, den * other.den);
    }
    TRational operator / (const TRational& other) const {
        if ((den == 0) || (other.num == 0)) {
            throw invalid_argument("Division by zero");
        }
        const int sign = (is_neg != other.is_neg) ? -1 : 1;
        return TRational(num * other.den * sign, den * other.num);
    }
    TRational operator - () const {
        return TRational(-num, den);
    }

    TRational operator + (const TLong& other) const {
        return TRational(num + other * den, den);
    }
    TRational operator - (const TLong& other) const {
        return TRational(num - other * den, den);
    }
    TRational operator * (const TLong& other) const {
        return TRational(num * other, den);
    }
    TRational operator / (const TLong& other) const {
        return TRational(num, den * other);
    }

    TRational operator + (const int& other) const {
        const int sign = (is_neg) ? -1 : 1;
        return TRational(num * sign + den * other, den);
    }
    TRational operator - (const int& other) const {
        const int sign = (is_neg) ? -1 : 1;
        return TRational(num * sign - den * other, den);
    }
    TRational operator * (const int& other) const {
        const int sign = (is_neg) ? -1 : 1;
        return TRational(num * other * sign, den);
    }
    TRational operator / (const int& other) const {
        const int sign = (is_neg) ? -1 : 1;
        return TRational(num * sign, den * other);
    }

    bool operator >= (const TRational& other) const {
        if (is_neg != other.is_neg) { return !is_neg; }
        if (is_neg) { // both < 0
            return other.num * den >= num * other.den;
        }
        else { // both > 0
            return num * other.den >= other.num * den;
        }
    }
    bool operator > (const TRational& other) const {
        if (is_neg != other.is_neg) { return !is_neg; }
        if (is_neg) { // both < 0
            return other.num * den > num * other.den;
        }
        else { // both > 0
            return num * other.den > other.num * den;
        }
    }
    bool operator == (const TRational& other) const {
        if (is_neg != other.is_neg) { return false; }
        return num * other.den == other.num * den;
    }
    bool operator <= (const TRational& other) const {
        return !(*this > other);
    }
    bool operator < (const TRational& other) const {
        return !(*this >= other);
    }
    bool operator != (const TRational& other) const {
        return !(*this == other);
    }

    bool operator >= (const TLong& other) const {
        if (is_neg != other.is_neg) { return !is_neg; }
        if (is_neg) { // both < 0
            return other * den >= num;
        }
        else { // both > 0
            return num >= other * den;
        }
    }
    bool operator > (const TLong& other) const {
        if (is_neg != other.is_neg) { return !is_neg; }
        if (is_neg) { // both < 0
            return other * den > num;
        }
        else { // both > 0
            return num > other * den;
        }
    }
    bool operator == (const TLong& other) const {
        if (is_neg != other.is_neg) { return false; }
        return num == other * den;
    }
    bool operator <= (const TLong& other) const {
        return !(*this > other);
    }
    bool operator < (const TLong& other) const {
        return !(*this >= other);
    }
    bool operator != (const TLong& other) const {
        return !(*this == other);
    }

    bool operator >= (const int& other) const {
        const bool sign = (other >= 0) ? true : false;
        if (is_neg != sign) { return !is_neg; }
        if (is_neg) { // both < 0
            return den * other >= num;
        }
        else { // both > 0
            return num >= den * other;
        }
    }
    bool operator > (const int& other) const {
        const bool sign = (other >= 0) ? true : false;
        if (is_neg != sign) { return !is_neg; }
        if (is_neg) { // both < 0
            return den * other > num;
        }
        else { // both > 0
            return num > den * other;
        }
    }
    bool operator == (const int& other) const {
        const bool sign = (other >= 0) ? true : false;
        if (is_neg != sign) { return false; }
        return num == den * num;
    }
    bool operator <= (const int& other) const {
        return !(*this > other);
    }
    bool operator < (const int& other) const {
        return !(*this >= other);
    }
    bool operator != (const int& other) const {
        return !(*this == other);
    }

    explicit operator double() const {
        return static_cast<double>(num.to_int()) / static_cast<double>(den.to_int());
    }
    [[nodiscard]] double sqrt() const {
        const TRational res(num, den);
        return std::sqrt(static_cast<double>(res));
    }
    friend ostream& operator << (ostream& os, const TRational& r) {
        if (r.is_neg) { os << "-"; }
        if (r.den != 1) {
            os << r.num << "/" << r.den;
        }
        else { os << r.num; }

        return os;
    }

};

template <typename T>
class Matrix {
private:
    size_t n_dim = 1, m_dim = 1; // Matrix has n_dim rows and m_dim columns
    vector<vector<T>> matrix; // Data of the matrix
public:

    explicit Matrix(const size_t n = 1, const size_t m = 1) {
        n_dim = n;
        m_dim = m;
        vector<T> zero_vector{};

        T zero_element;
        if constexpr (is_same_v<T, TRational>) {
            zero_element = TRational(0, 1);
        }
        else {
            zero_element = static_cast<T>(0.0);
        }

        zero_vector.assign(m_dim, zero_element);
        matrix.assign(n_dim, zero_vector);
    }

    explicit Matrix(const vector<vector<T>>& mat_data) {
        if (mat_data.empty()) {
            throw invalid_argument("Matrix data cannot be empty.");
        }

        n_dim = mat_data.size();
        m_dim = mat_data[0].size();
        matrix = mat_data;

        for (const auto& row : mat_data) {
            if (row.size() != m_dim) {
                throw invalid_argument("All rows must have the same number of columns.");
            }
        }
    }

    T& operator()(size_t i, size_t j) {
        return matrix[i][j];
    }
    const T& operator()(size_t i, size_t j) const {
        return matrix[i][j];
    }
    Matrix<T> operator + (const Matrix<T>& other) const {
        if (n_dim != other.n_dim || m_dim != other.m_dim) {
            throw invalid_argument("Matrices must have the same dimensions for addition.");
        }

        vector<vector<T>> result_data(this->n_dim, vector<T>(this->m_dim));

        for (size_t i = 0; i < n_dim; ++i) {
            for (size_t j = 0; j < m_dim; ++j) {
                result_data[i][j] = matrix[i][j] + other.matrix[i][j];
            }
        }

        return Matrix<T>(result_data);
    }
    Matrix<T> operator - (const Matrix<T>& other) const {
        if (n_dim != other.n_dim || m_dim != other.m_dim) {
            throw invalid_argument("Matrices must have the same dimensions for subtraction.");
        }

        vector<vector<T>> result_data(this->n_dim, vector<T>(this->m_dim));

        for (size_t i = 0; i < n_dim; ++i) {
            for (size_t j = 0; j < m_dim; ++j) {
                result_data[i][j] = matrix[i][j] - other.matrix[i][j];
            }
        }

        return Matrix<T>(result_data);
    }
    Matrix<T> operator * (const Matrix<T>& other) const {
        if (m_dim != other.n_dim) {
            throw invalid_argument("Number of columns of the first matrix must equal the number of rows of the second matrix.");
        }

        Matrix<T> result_data(n_dim, other.m_dim);
        for (size_t i = 0; i < n_dim; ++i) {
            for (size_t j = 0; j < other.m_dim; ++j) {
                for (size_t k = 0; k < m_dim; ++k) {
                    result_data(i, j) = result_data(i, j) + matrix[i][k] * other.matrix[k][j];
                }
            }
        }

        return result_data;
    }
    Matrix<T> operator * (const double& scalar) const {
        Matrix<T> result_data(n_dim, m_dim);

        for (size_t i = 0; i < n_dim; i++) {
            for (size_t j = 0; j < m_dim; j++) {
                result_data(i, j) = matrix[i][j] * scalar;
            }
        }
        return result_data;
    }
    Matrix<T> operator * (const TRational& scalar) const {
        Matrix<T> result_data(n_dim, m_dim);

        for (size_t i = 0; i < n_dim; i++) {
            for (size_t j = 0; j < m_dim; j++) {
                result_data(i, j) = matrix[i][j] * scalar;
            }
        }

        return result_data;
    }

    [[nodiscard]] Matrix<T> transpose() const {
        Matrix<T> result(m_dim, n_dim);
        for (size_t i = 0; i < m_dim; i++) {
            for (size_t j = 0; j < n_dim; j++) {
                result(i, j) = matrix[j][i];
            }
        }
        return result;
    }
    [[nodiscard]] Matrix<T> _submatrix(const size_t exclude_row, const size_t exclude_col) const {
        vector<vector<T>> submatrix_data;

        for (size_t i = 0; i < n_dim; ++i) {
            if (i == exclude_row) continue;
            vector<T> submatrix_row;
            for (size_t j = 0; j < m_dim; ++j) {
                if (j == exclude_col) continue;
                submatrix_row.push_back(matrix[i][j]);
            }
            submatrix_data.push_back(submatrix_row);
        }
        return Matrix<T>(submatrix_data);
    }
    [[nodiscard]] T determinant() const {
        if (n_dim != m_dim) {
            throw invalid_argument("Matrix must be square");
        }
        if (n_dim == 1) { return matrix[0][0]; }
        if (n_dim == 2) { return matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0]; }

        T det = T(0);
        for (size_t j = 0; j < m_dim; ++j) {
            Matrix<T> submatrix = _submatrix(0, j);
            int Aij_sign = (j % 2 == 0 ? 1 : -1);
            det = det + matrix[0][j] * Aij_sign * submatrix.determinant();
        }
        return det;
    }
    [[nodiscard]] Matrix<T> strassen_method(const Matrix<T> other) const {
        if ((n_dim != 2) || (m_dim != 2)) {
            throw logic_error("Matrix must be 2 x 2!");
        }
        Matrix<T> res(2, 2);
        T P1 = (matrix[0][0] + matrix[1][1]) * (other(0, 0) + other(1, 1));
        T P2 = (matrix[1][0] + matrix[1][1]) * other(0, 0);
        T P3 = matrix[0][0] * (other(0, 1) - other(1, 1));
        T P4 = matrix[1][1] * (other(1, 0) - other(0, 0));
        T P5 = (matrix[0][0] + matrix[0][1]) * other(1, 1);
        T P6 = (matrix[1][0] - matrix[0][0]) * (other(0, 0) + other(0, 1));
        T P7 = (matrix[0][1] - matrix[1][1]) * (other(1, 0) + other(1, 1));

        res(0, 0) = P1 + P4 - P5 + P7; // C11
        res(0, 1) = P3 + P5;           // C12
        res(1, 0) = P2 + P4;           // C21
        res(1, 1) = P1 - P2 + P3 + P6; // C22

        return res;
    }
    [[nodiscard]] double norm() const {
        double norm = 0.0;
        for (size_t i = 0; i < n_dim; ++i) {
            for (size_t j = 0; j < m_dim; ++j) {
                T el = matrix[i][j] * matrix[i][j];
                norm += static_cast<double>(el);
            }
        }
        return sqrt(norm);
    }
    friend ostream& operator << (ostream& os, const Matrix& m) {
        os << "[";
        int i_counter = 1;
        for (vector<T> row : m.matrix) {
            os << "[";
            for (int i = 0; i < row.size(); ++i) {
                if (i == row.size() - 1) {
                    os << row[i];
                }
                else {
                    os << row[i] << ", ";
                }
            }
            if (i_counter != m.n_dim) {
                os << "],\n ";
                i_counter++;
            }
            else {
                os << "]";
            }
        }
        os << "]";
        return os;
    }
};

void TLong_test(const vector<string>& v1, const vector<string>& v2) {
    for (const auto& i : v1) {
        for (const auto& j : v2) {
            TLong num1(i), num2(j);
            cout << boolalpha;
            cout << "TLong + : " << num1 << " + " << num2 << " = " << num1 + num2 << endl;
            cout << "TLong - : " << num1 << " - " << num2 << " = " << num1 - num2 << endl;
            cout << "TLong * : " << num1 << " * " << num2 << " = " << num1 * num2 << endl;
            cout << "TLong / : " << num1 << " / " << num2 << " = " << num1 / num2 << endl;
            cout << "TLong % : " << num1 << " % " << num2 << " = " << num1 % num2 << endl;
            cout << "TLong < : " << num1 << " < " << num2 << " = " << (num1 < num2) << endl;
            cout << "TLong >= : " << num1 << " >= " << num2 << " = " << (num1 >= num2) << endl;
            cout << "TLong == : " << num1 << " == " << num2 << " = " << (num1 == num2) << endl << endl;;
        }
    }
}
void TRational_test(const vector<ll>& v11, const vector<ll>& v12, const vector<ll>& v21, const vector<ll>& v22) {
    for (const auto& i1 : v11) {
        for (const auto& i2 : v12) {
            for (const auto& j1 : v21) {
                for (const auto& j2 : v22) {
                    TRational num1(i1, i2);
                    TRational num2(j1, j2);
                    cout << "TRational + : " << num1 << " + " << num2 << " = " << num1 + num2 << endl;
                    cout << "TRational - : " << num1 << " - " << num2 << " = " << num1 - num2 << endl;
                    cout << "TRational * : " << num1 << " * " << num2 << " = " << num1 * num2 << endl;
                    cout << "TRational / : " << num1 << " / " << num2 << " = " << num1 / num2 << endl;
                    cout << "TRational < : " << num1 << " < " << num2 << " = " << (num1 < num2) << endl;
                    cout << "TRational >= : " << num1 << " >= " << num2 << " = " << (num1 >= num2) << endl;
                    cout << "TRational == : " << num1 << " == " << num2 << " = " << (num1 == num2) << endl << endl;
                }
            }
        }
    }
}

template <typename T>
void non_quad_Matrix_test(const vector<vector<T>> m1, const vector<vector<T>> m2) {
    Matrix<T> mat1(m1);
    Matrix<T> mat2(m2);

    cout << "Matrix index :\n" << "mat1(0, 2)" << " = " << mat1(0, 2) << endl;
    // cout <<"Matrix + :\n" << mat1 << "\n+\n" << mat2 << "\n=\n" << (mat1 + mat2) << endl << endl << endl;
    // cout <<"Matrix - :\n" << mat1 << "\n-\n" << mat2 << "\n=\n" << (mat1 - mat2) << endl << endl << endl;
    cout << "Matrix * :\n" << mat1 << "\n*\n" << mat2 << "\n=\n" << (mat1 * mat2) << endl << endl << endl;
    cout << "Matrix 10*M :\n" << "10 * \n" << mat1 << "\n=\n" << mat1 * 10 << endl << endl << endl;
    cout << "Matrix transposition 1 :\n" << mat1 << "^T\n=\n" << mat1.transpose() << endl << endl << endl;
    cout << "Matrix transposition 2 :\n" << mat2 << "^T\n=\n" << mat2.transpose() << endl << endl << endl;
    cout << "Matrix norm 1 :\n" << mat1 << "\n=\n" << mat1.norm() << endl << endl << endl;
    cout << "Matrix norm 2 :\n" << mat2 << "\n=\n" << mat2.norm() << endl << endl << endl;
}
template <typename T>
void quad_matrix_test(const vector<vector<T>> m1, const vector<vector<T>> m2) {
    Matrix<T> mat1(m1);
    Matrix<T> mat2(m2);

    cout << "Matrix index :\n" << "mat1(0, 2)" << " = " << mat1(0, 2) << endl;
    cout << "Matrix + :\n" << mat1 << "\n+\n" << mat2 << "\n=\n" << (mat1 + mat2) << endl << endl << endl;
    cout << "Matrix - :\n" << mat1 << "\n-\n" << mat2 << "\n=\n" << (mat1 - mat2) << endl << endl << endl;
    cout << "Matrix * :\n" << mat1 << "\n*\n" << mat2 << "\n=\n" << (mat1 * mat2) << endl << endl << endl;
    cout << "Matrix 10*M :\n" << "10 * \n" << mat1 << "\n=\n" << mat1 * 10 << endl << endl << endl;
    cout << "Matrix transposition 1 :\n" << mat1 << "^T\n=\n" << mat1.transpose() << endl << endl << endl;
    cout << "Matrix transposition 2 :\n" << mat2 << "^T\n=\n" << mat2.transpose() << endl << endl << endl;
    cout << "Matrix determinant 1 :\n" << mat1 << "\n=\n" << mat1.determinant() << endl << endl << endl;
    cout << "Matrix determinant 2 :\n" << mat2 << "\n=\n" << mat2.determinant() << endl << endl << endl;
    cout << "Matrix norm 1 :\n" << mat1 << "\n=\n" << mat1.norm() << endl << endl << endl;
    cout << "Matrix norm 2 :\n" << mat2 << "\n=\n" << mat2.norm() << endl << endl << endl;
}
template <typename T>
void strassen_method_test(const vector<vector<T>>& m1, const vector<vector<T>>& m2) {
    Matrix<T> mat1(m1);
    Matrix<T> mat2(m2);
    cout << "Matrix Strassen method :\n" << mat1 << "\n*\n" << mat2 << "\n=\n" << mat1.strassen_method(mat2) << endl << endl << endl;
}




int main() {
    const vector<vector<double>> m1 = { {1.0, -2.5, 8.0, 7.2},
                                        {2.5, -3.0, -5.0, 0.0},
                                        {3.0, -1.4, 4.0, 1.2} };
    const vector<vector<double>> m2 = { {4.0, -1.0, 3.0},
                                        {-2.5, 6.2, 0.0},
                                        {5.8, -1.0, 2.8},
                                        {7.6, -1.0, 0.0} };
    const vector<vector<TRational>> m3 = { {TRational(2, 5), TRational(3, 10), TRational(7, 4)},
                                            {TRational(-1, 5), TRational(-3, 8), TRational(6, 5)},
                                            {TRational(1), TRational(-1), TRational(2, 9)} };
    const vector<vector<TRational>> m4 = { {TRational(-2), TRational(6, 2), TRational(1)},
                                            {TRational(0), TRational(-3, 8), TRational(2, 5)},
                                            {TRational(1), TRational(-1), TRational(1, 4)} };
    const vector<vector<TRational>> m5 = { {TRational(2, 5), TRational(3, 10)},
                                            {TRational(-1, 5), TRational(-3, 8)} };
    const vector<vector<TRational>> m6 = { {TRational(-2), TRational(6, 2)},
                                            {TRational(0), TRational(-3, 8)} };

    TLong_test({ "100", "-100" }, { "25", "1" });
    TRational_test({ 5, -5 }, { 12 }, { 7, -7 }, { 10 });
    non_quad_Matrix_test(m1, m2);
    quad_matrix_test(m3, m4);
    strassen_method_test(m5, m6);
    return 0;
}


#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <chrono>

class BigInt {
private:
    std::string value;
    bool isNegative;

public:
    bool isLessThan(const BigInt& other) const {
        if (isNegative != other.isNegative) return isNegative;

        if (value.length() != other.value.length()) {
            return (value.length() < other.value.length()) ^ isNegative;
        }

        return (value < other.value) ^ isNegative;
    }

    // Constructor to initialize BigInt from string
    BigInt(std::string val) {
        if (val[0] == '-') {
            isNegative = true;
            value = val.substr(1);
        }
        else {
            isNegative = false;
            value = val;
        }
        value = removeLeadingZeros(value);
    }

    BigInt() : value("0"), isNegative(false) {}

    // Helper function to remove leading zeros
    static std::string removeLeadingZeros(const std::string& str) {
        size_t start = str.find_first_not_of('0');
        if (start == std::string::npos) return "0";
        return str.substr(start);
    }

    const std::string& getValue() const { return value; }
    bool getIsNegative() const { return isNegative; }

    BigInt shiftLeft(int n) const {
        if (value == "0") return *this;
        return BigInt(value + std::string(n, '0'));
    }

    // Get a substring of the BigInt value for use in Karatsuba
    BigInt getSubBigInt(int start, int length = -1) const {
        // If start is out of bounds, return 0
        if (start >= value.size()) return BigInt("0");

        // Adjust length to avoid out-of-bounds access
        if (length == -1 || start + length > value.size()) {
            length = value.size() - start;
        }

        // Return the BigInt created from the specified substring
        return BigInt(value.substr(start, length));
    }

    bool operator!=(const BigInt& other) const {
        return isNegative != other.isNegative || value != other.value;
    }

    // Addition of two BigInts
    BigInt operator+(const BigInt& other) const {
        if (isNegative == other.isNegative) {
            std::string result;
            int carry = 0, sum = 0;
            int i = value.size() - 1, j = other.value.size() - 1;

            while (i >= 0 || j >= 0 || carry) {
                int digit1 = i >= 0 ? value[i--] - '0' : 0;
                int digit2 = j >= 0 ? other.value[j--] - '0' : 0;
                sum = digit1 + digit2 + carry;
                carry = sum / 10;
                result.push_back(sum % 10 + '0');
            }
            if (isNegative) result.push_back('-');
            std::reverse(result.begin(), result.end());
            return BigInt(result);
        }
        return *this - (-other);
    }

    // Subtraction of two BigInts
    BigInt operator-(const BigInt& other) const {
        // Case 1: If the signs are different, convert to addition
        if (other.value == "0") {
            return *this;
        }

        if (isNegative != other.isNegative) {
            return *this + (-other);
        }

        // Case 2: If both numbers are equal, return zero
        if (value == other.value) {
            return BigInt("0");
        }

        // Case 3: Handle both numbers negative (e.g., -a - (-b) = b - a)
        if (isNegative && other.isNegative) {
            if (*this > other) {
                return -(-(*this) - (-(other)));
            }
            else {
                return (-(other)-(-(*this)));
            }


            // Otherwise, proceed with subtraction where `*this` has a greater absolute value
        }

        // Case 4: Handle both numbers positive, but *this is smaller than `other`
        if (!isNegative && isLessThan(other)) {
            // If `*this` is smaller, reverse the order and negate the result
            return -(other - *this);
        }

        // Case 5: Perform actual subtraction when both numbers have the same sign
        // and `*this` has a larger or equal absolute value
        std::string result;
        int borrow = 0, diff = 0;
        int i = value.size() - 1, j = other.value.size() - 1;

        while (i >= 0 || j >= 0 || borrow) {
            int digit1 = i >= 0 ? value[i--] - '0' : 0;
            int digit2 = j >= 0 ? other.value[j--] - '0' : 0;
            diff = digit1 - digit2 - borrow;

            if (diff < 0) {
                diff += 10;
                borrow = 1;
            }
            else {
                borrow = 0;
            }

            result.push_back(diff + '0');
        }

        // Remove leading zeros and reverse the result
        std::reverse(result.begin(), result.end());
        result = removeLeadingZeros(result);

        // Ensure the result sign is correct
        if (isNegative && !(result == "0")) {
            result.insert(result.begin(), '-');
        }

        return BigInt(result);
    }



    // Negation of BigInt
    BigInt operator-() const {
        BigInt result = *this;
        if (result.value != "0") result.isNegative = !result.isNegative;
        return result;
    }

    // Multiplication of two BigInts
    BigInt operator*(const BigInt& other) const {
        BigInt product("0");
        for (int i = other.value.size() - 1; i >= 0; --i) {
            int carry = 0;
            std::string current;
            for (int k = 0; k < other.value.size() - 1 - i; ++k) current.push_back('0');

            for (int j = value.size() - 1; j >= 0 || carry; --j) {
                int digit1 = j >= 0 ? value[j] - '0' : 0;
                int digit2 = other.value[i] - '0';
                int mul = digit1 * digit2 + carry;
                carry = mul / 10;
                current.push_back(mul % 10 + '0');
            }
            std::reverse(current.begin(), current.end());
            product = product + BigInt(current);
        }
        product.isNegative = isNegative != other.isNegative;
        return product;
    }

    BigInt operator/(const BigInt& other) const {
        if (other.value == "0") throw std::runtime_error("Division by zero!");

        BigInt dividend = *this;
        BigInt divisor = other;
        BigInt quotient("0");
        BigInt one("1");

        dividend.isNegative = divisor.isNegative = false;

        // Optimized division using repeated subtraction
        if (dividend.isLessThan(divisor)) {
            return BigInt("0");
        }

        while (!dividend.isLessThan(divisor)) {
            BigInt currentDivisor = divisor;
            BigInt currentQuotient("1");

            while (!dividend.isLessThan(currentDivisor.shiftLeft(1))) {
                currentDivisor = currentDivisor.shiftLeft(1);
                currentQuotient = currentQuotient.shiftLeft(1);
            }

            dividend = dividend - currentDivisor;
            quotient = quotient + currentQuotient;
        }

        quotient.isNegative = isNegative != other.isNegative;
        return quotient;
    }

    // Modulus operator
    BigInt operator%(const BigInt& other) const {
        if (other.value == "0") throw std::runtime_error("Modulo by zero!");

        BigInt dividend = *this;
        BigInt divisor = other;
        dividend.isNegative = divisor.isNegative = false;

        // Using the division result to calculate remainder more efficiently
        BigInt quotient = dividend / divisor;

        BigInt remainder = dividend - (quotient * divisor);

        return remainder.isNegative ? -remainder : remainder;
    }

    // Division-assignment operator
    BigInt& operator/=(const BigInt& other) {
        if (other.value == "0") throw std::runtime_error("Division by zero!");

        *this = *this / other;
        return *this;
    }



    // Overload stream output
    friend std::ostream& operator<<(std::ostream& os, const BigInt& bigint) {
        if (bigint.isNegative && bigint.value != "0") os << "-";
        os << bigint.value;
        return os;
    }

    // Equality operator
    bool operator==(const BigInt& other) const {
        return isNegative == other.isNegative && value == other.value;
    }

    // Less-than operator
    bool operator<(const BigInt& other) const {
        if (isNegative != other.isNegative) {
            return isNegative;  // Negative numbers are always less than positive
        }

        // If both are positive, compare absolute values normally
        // If both are negative, reverse the comparison
        bool result;
        if (value.length() != other.value.length()) {
            result = value.length() < other.value.length();
        }
        else {
            result = value < other.value;
        }

        return isNegative ? !result : result;  // Invert result if both are negative
    }

    bool operator>(const BigInt& other) const {
        if (isNegative != other.isNegative) {
            return isNegative;  // Negative numbers are always less than positive
        }

        // If both are positive, compare absolute values normally
        // If both are negative, reverse the comparison
        bool result;
        if (value.length() != other.value.length()) {
            result = value.length() > other.value.length();
        }
        else {
            result = value > other.value;
        }

        return isNegative ? !result : result;  // Invert result if both are negative
    }
};


class BigNat {
private:
    std::string value; // Store the big integer as a string to handle large sizes

    // Helper function to remove leading zeros
    static std::string removeLeadingZeros(const std::string& str) {
        size_t start = str.find_first_not_of('0');
        if (start == std::string::npos) return "0";
        return str.substr(start);
    }

    // Helper function to check if this BigNat is less than another
    bool isLessThan(const BigNat& other) const {
        if (value.length() != other.value.length()) {
            return value.length() < other.value.length();
        }
        return value < other.value;
    }

public:
    // Constructor to initialize BigNat from string
    BigNat(std::string val) : value(removeLeadingZeros(val)) {}

    // Constructor to initialize BigNat from integer
    BigNat(int val) : value(std::to_string(val)) {}

    // Addition of two BigNats
    BigNat operator+(const BigNat& other) const {
        std::string result;
        int carry = 0, sum = 0;
        int i = value.size() - 1, j = other.value.size() - 1;

        while (i >= 0 || j >= 0 || carry) {
            int digit1 = i >= 0 ? value[i--] - '0' : 0;
            int digit2 = j >= 0 ? other.value[j--] - '0' : 0;
            sum = digit1 + digit2 + carry;
            carry = sum / 10;
            result.push_back(sum % 10 + '0');
        }

        std::reverse(result.begin(), result.end());
        return BigNat(result);
    }

    // Subtraction of two BigNats (assuming *this >= other)
    BigNat operator-(const BigNat& other) const {
        if (isLessThan(other)) throw std::invalid_argument("Result would be negative");

        std::string result;
        int borrow = 0, diff = 0;
        int i = value.size() - 1, j = other.value.size() - 1;

        while (i >= 0 || j >= 0 || borrow) {
            int digit1 = i >= 0 ? value[i--] - '0' : 0;
            int digit2 = j >= 0 ? other.value[j--] - '0' : 0;
            diff = digit1 - digit2 - borrow;
            if (diff < 0) {
                diff += 10;
                borrow = 1;
            }
            else {
                borrow = 0;
            }
            result.push_back(diff + '0');
        }

        std::reverse(result.begin(), result.end());
        return BigNat(removeLeadingZeros(result));
    }

    // Multiplication of two BigNats
    BigNat operator*(const BigNat& other) const {
        BigNat product("0");
        for (int i = other.value.size() - 1; i >= 0; --i) {
            int carry = 0;
            std::string current;
            for (int k = 0; k < other.value.size() - 1 - i; ++k) current.push_back('0');

            for (int j = value.size() - 1; j >= 0 || carry; --j) {
                int digit1 = j >= 0 ? value[j] - '0' : 0;
                int digit2 = other.value[i] - '0';
                int mul = digit1 * digit2 + carry;
                carry = mul / 10;
                current.push_back(mul % 10 + '0');
            }
            std::reverse(current.begin(), current.end());
            product = product + BigNat(current);
        }
        return product;
    }

    // Division of two BigNats (integer division)
    BigNat operator/(const BigNat& other) const {
        if (other.value == "0") throw std::runtime_error("Division by zero!");

        BigNat dividend = *this;
        BigNat divisor = other;
        BigNat quotient("0");
        BigNat one("1");

        while (!dividend.isLessThan(divisor)) {
            dividend = dividend - divisor;
            quotient = quotient + one;
        }

        return quotient;
    }

    // Overload stream output for easy display
    friend std::ostream& operator<<(std::ostream& os, const BigNat& bignat) {
        os << bignat.value;
        return os;
    }
};

BigInt gcd(const BigInt& a, const BigInt& b) {
    BigInt absA = a.getIsNegative() ? -a : a;
    BigInt absB = b.getIsNegative() ? -b : b;

    while (absB.getValue() != "0") {
        BigInt temp = absB;
        absB = absA % absB; // Assuming BigInt class has a modulo operator
        absA = temp;
    }
    return absA;
}

class BigRational {
private:
    BigInt numerator;
    BigInt denominator;

    void reduce() {
        BigInt gcdValue = gcd(numerator, denominator);  // Find GCD of numerator and denominator
        numerator /= gcdValue;                          // Divide numerator by GCD
        denominator /= gcdValue;                        // Divide denominator by GCD

        // Ensure the denominator is always positive
        if (denominator.getIsNegative()) {
            numerator = -numerator;
            denominator = -denominator;
        }
    }

public:
    BigRational(const BigInt& num, const BigInt& den) : numerator(num), denominator(den) {
        if (den.getValue() == "0") {
            throw std::invalid_argument("Denominator cannot be zero.");
        }
        reduce();
    }

    BigRational operator+(const BigRational& other) const {
        BigInt commonDenominator = denominator * other.denominator;
        BigInt newNumerator = (numerator * other.denominator) + (other.numerator * denominator);
        return BigRational(newNumerator, commonDenominator);
    }

    BigRational operator-(const BigRational& other) const {
        BigInt commonDenominator = denominator * other.denominator;
        BigInt newNumerator = (numerator * other.denominator) - (other.numerator * denominator);
        return BigRational(newNumerator, commonDenominator);
    }

    BigRational operator*(const BigRational& other) const {
        BigInt newNumerator = numerator * other.numerator;
        BigInt newDenominator = denominator * other.denominator;
        return BigRational(newNumerator, newDenominator);
    }

    BigRational operator/(const BigRational& other) const {
        if (other.numerator.getValue() == "0") throw std::runtime_error("Division by zero!");
        BigInt newNumerator = numerator * other.denominator;
        BigInt newDenominator = denominator * other.numerator;
        return BigRational(newNumerator, newDenominator);
    }

    friend std::ostream& operator<<(std::ostream& os, const BigRational& bigrat) {
        os << bigrat.numerator << "/" << bigrat.denominator;
        return os;
    }
};

template <typename T>
T gcd(T a, T b) {
    while (b != T(0)) {
        T temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

template <typename T>
class TBigRational {
private:
    T numerator;
    T denominator;

    // Helper function to reduce the fraction by dividing both numerator and denominator by their GCD
    void reduce() {
        T gcdVal = gcd(numerator, denominator);
        numerator /= gcdVal;
        denominator /= gcdVal;

        // Ensure the denominator is positive
        if (denominator < T(0)) {
            numerator = -numerator;
            denominator = -denominator;
        }
    }

public:
    // Constructors
    TBigRational(const T& num = T(0), const T& den = T(1))
        : numerator(num), denominator(den) {
        if (denominator == T(0)) throw std::invalid_argument("Denominator cannot be zero");
        reduce();
    }

    // Addition
    TBigRational operator+(const TBigRational& other) const {
        T commonDenom = denominator * other.denominator;
        T newNumerator = (numerator * other.denominator) + (other.numerator * denominator);
        return TBigRational(newNumerator, commonDenom);
    }

    // Subtraction
    TBigRational operator-(const TBigRational& other) const {
        T commonDenom = denominator * other.denominator;
        T newNumerator = (numerator * other.denominator) - (other.numerator * denominator);
        return TBigRational(newNumerator, commonDenom);
    }

    // Multiplication
    TBigRational operator*(const TBigRational& other) const {
        T newNumerator = numerator * other.numerator;
        T newDenominator = denominator * other.denominator;
        return TBigRational(newNumerator, newDenominator);
    }

    // Division
    TBigRational operator/(const TBigRational& other) const {
        if (other.numerator == T(0)) throw std::runtime_error("Division by zero");
        T newNumerator = numerator * other.denominator;
        T newDenominator = denominator * other.numerator;
        return TBigRational(newNumerator, newDenominator);
    }

    // Output stream operator for printing
    friend std::ostream& operator<<(std::ostream& os, const TBigRational& bigrat) {
        os << bigrat.numerator << "/" << bigrat.denominator;
        return os;
    }

    // Accessor methods for numerator and denominator
    T getNumerator() const { return numerator; }
    T getDenominator() const { return denominator; }
};

BigInt karatsuba(const BigInt& x, const BigInt& y) {
    bool isXNegative = x.getIsNegative();
    bool isYNegative = y.getIsNegative();

    // Base case: if either number is a single digit
    if (x.getValue().size() == 1 || y.getValue().size() == 1) {
        return x * y;
    }

    // Make strings the same length by padding with zeros if necessary
    int n = std::max(x.getValue().size(), y.getValue().size());
    int half = n / 2;

    BigInt x1 = x.getSubBigInt(0, x.getValue().size() - half);  // High part of x
    BigInt x0 = x.getSubBigInt(x.getValue().size() - half);    // Low part of x
    BigInt y1 = y.getSubBigInt(0, y.getValue().size() - half);  // High part of y
    BigInt y0 = y.getSubBigInt(y.getValue().size() - half);    // Low part of y

    // Recursively calculate three products
    BigInt z2 = karatsuba(x1, y1);
    BigInt z0 = karatsuba(x0, y0);
    BigInt z1 = karatsuba(x1 + x0, y1 + y0) - z2 - z0;

    // Shift z2 and z1 appropriately
    z2 = z2.shiftLeft(2 * half);
    z1 = z1.shiftLeft(half);

    // Combine results
    BigInt result = z2 + z1 + z0;

    // Logic to remove half zeros if either x or y is smaller than half
    if (x.getValue().size() < half || y.getValue().size() < half) {
        int zerosToRemove = half;  // Calculate half of `half`
        std::string resultValue = result.getValue();
        size_t length = resultValue.size();

        // Remove zeros if present at the end of the result
        size_t nonZeroPos = resultValue.find_last_not_of('0');
        if (nonZeroPos != std::string::npos && length - nonZeroPos - 1 >= zerosToRemove) {
            resultValue.erase(length - zerosToRemove);
        }

        result = BigInt(resultValue); // Update the result with trimmed value
    }

    if (isXNegative != isYNegative) {
        result = -result;
    }

    return result;
}

BigInt toom3Multiply(const BigInt& a, const BigInt& b) {
    // Step 1: Check signs
    bool isANegative = a.getIsNegative();
    bool isBNegative = b.getIsNegative();

    // Convert `a` and `b` to positive for the multiplication if they are negative
    BigInt absA = isANegative ? -a : a;
    BigInt absB = isBNegative ? -b : b;

    int n = std::max(absA.getValue().size(), absB.getValue().size());
    int m = std::min(absA.getValue().size(), absB.getValue().size());
    if (m <= 3) {
        BigInt result = absA * absB;
        return isANegative != isBNegative ? -result : result;  // Adjust sign for small inputs
    }

    // Determine part sizes
    int baseSize = n / 3;
    int remainder = n % 3;
    int partSize2a = baseSize;
    int partSize3a = baseSize;
    int partSize2b = baseSize;
    int partSize3b = baseSize;
    if (remainder > 0) partSize2a++;
    if (remainder > 0) partSize3a++;
    if (remainder > 0) partSize2b++;
    if (remainder > 0) partSize3b++;
    int partSize1a = std::max(0, static_cast<int>(absA.getValue().size()) - 2 * partSize2a);
    //std::cout << "partsize1a: " << partSize1a << std::endl;
    int partSize1b = std::max(0, static_cast<int>(absB.getValue().size()) - 2 * partSize2b);
    //std::cout << "partsize1b: " << partSize1b << std::endl;
    if (partSize1a == 0) {
        partSize2a = std::max(0, static_cast<int>(absA.getValue().size() - partSize2a));
    }
    if (partSize1b == 0) {
        partSize2b = std::max(0, static_cast<int>(absB.getValue().size() - partSize2b));
    }
    //std::cout << "partsize2b: " << partSize2b << std::endl;
    // Split `a` and `b` into parts
    BigInt a0 = absA.getSubBigInt(partSize1a + partSize2a, partSize3a);
    //std::cout << "a0: " << a0 << std::endl;
    BigInt a1 = absA.getSubBigInt(partSize1a, partSize2a);
    //std::cout << "a1: " << a1 << std::endl;
    BigInt a2 = absA.getSubBigInt(0, partSize1a);
    //std::cout << "a2: " << a2 << std::endl;
    BigInt b0 = (partSize1b + partSize2b <= absB.getValue().size()) ? absB.getSubBigInt(partSize1b + partSize2b, partSize3b) : BigInt("0");
    //std::cout << "b0: " << b0 << std::endl;
    BigInt b1 = (partSize1b <= absB.getValue().size()) ? absB.getSubBigInt(partSize1b, partSize2b) : BigInt("0");
    //std::cout << "b1: " << b1 << std::endl;
    BigInt b2 = absB.getSubBigInt(0, partSize1b);
    //std::cout << "b2: " << b2 << std::endl;
    // Evaluate polynomials
    BigInt p0 = a0;
    //std::cout << "p0: " << p0 << std::endl;
    BigInt p1 = a0 + a1 + a2;
    //std::cout << "p1: " << p1 << std::endl;
    BigInt p2 = (a0 - a1) + a2;
    //std::cout << "p2: " << p2 << std::endl;
    BigInt p3 = a0 - (BigInt("2") * a1) + (BigInt("4") * a2);
    //std::cout << "p3: " << p3 << std::endl;
    BigInt p4 = a2;
    //std::cout << "p4: " << p4 << std::endl;

    BigInt q0 = b0;
    //std::cout << "q0: " << q0 << std::endl;
    BigInt q1 = b0 + b1 + b2;
    //std::cout << "q1: " << q1 << std::endl;
    BigInt q2 = b0 - b1 + b2;
    //std::cout << "q2: " << q2 << std::endl;
    BigInt q3 = b0 - (BigInt("2") * b1) + (BigInt("4") * b2);
    //std::cout << "q3: " << q3 << std::endl;
    BigInt q4 = b2;
    //std::cout << "q4: " << q4 << std::endl;

    // Recursive multiplications
    BigInt r0 = toom3Multiply(p0, q0);
    BigInt r1 = toom3Multiply(p1, q1);
    BigInt r2 = toom3Multiply(p2, q2);
    BigInt r3 = toom3Multiply(p3, q3);
    BigInt r4 = toom3Multiply(p4, q4);

    BigInt r00 = r0;
    //std::cout << "r00: " << r00 << std::endl;
    BigInt r04 = r4;
    //std::cout << "r04: " << r04 << std::endl;
    BigInt r03 = (r3 - r1) / BigInt("3");
    //std::cout << "r03: " << r03 << std::endl;
    BigInt r01 = (r1 - r2) / BigInt("2");
    //std::cout << "r01: " << r01 << std::endl;
    BigInt r02 = (r2 - r0);
    //std::cout << "r02: " << r02 << std::endl;

    r03 = ((r02 - r03) / BigInt("2")) + (BigInt("2") * r04);
    //std::cout << "r03 (refined): " << r03 << std::endl;
    r02 = r02 + r01 - r04;
    //std::cout << "r02 (refined): " << r02 << std::endl;
    r01 = r01 - r03;
    //std::cout << "r01 (refined): " << r01 << std::endl;

    BigInt result = r00
        + (r01.shiftLeft(partSize3a))
        + (r02.shiftLeft(2 * partSize3a))
        + (r03.shiftLeft(3 * partSize3a))
        + (r04.shiftLeft(4 * partSize3a));
    //std::cout << "result: " << result << std::endl;

    // Adjust sign based on original signs of `a` and `b`
    if (isANegative != isBNegative) {
        result = -result;
    }

    return result;
}


BigInt toom5Multiply(const BigInt& a, const BigInt& b) {
    // Step 1: Check signs
    bool isANegative = a.getIsNegative();
    bool isBNegative = b.getIsNegative();

    // Convert `a` and `b` to positive for the multiplication if they are negative
    BigInt absA = isANegative ? -a : a;
    BigInt absB = isBNegative ? -b : b;

    int n = std::max(absA.getValue().size(), absB.getValue().size());
    int m = std::min(absA.getValue().size(), absB.getValue().size());
    if (m <= 3) {
        BigInt result = absA * absB;
        return isANegative != isBNegative ? -result : result;  // Adjust sign for small inputs
    }

    // Determine part sizes
    int baseSize = n / 5;
    int remainder = n % 5;

    // Calculate individual part sizes
    int partSize4 = baseSize + (remainder > 0 ? 1 : 0);
    int partSize3 = baseSize + (remainder > 1 ? 1 : 0);
    int partSize2 = baseSize + (remainder > 2 ? 1 : 0);
    int partSize1 = baseSize + (remainder > 3 ? 1 : 0);
    int partSize0 = absA.getValue().size() - 4 * baseSize;

    // Split `a` and `b` into five parts
    BigInt a0 = absA.getSubBigInt(partSize1 + partSize2 + partSize3 + partSize4, partSize0);
    BigInt a1 = absA.getSubBigInt(partSize2 + partSize3 + partSize4, partSize1);
    BigInt a2 = absA.getSubBigInt(partSize3 + partSize4, partSize2);
    BigInt a3 = absA.getSubBigInt(partSize4, partSize3);
    BigInt a4 = absA.getSubBigInt(0, partSize4);

    BigInt b0 = absB.getSubBigInt(partSize1 + partSize2 + partSize3 + partSize4, partSize0);
    BigInt b1 = absB.getSubBigInt(partSize2 + partSize3 + partSize4, partSize1);
    BigInt b2 = absB.getSubBigInt(partSize3 + partSize4, partSize2);
    BigInt b3 = absB.getSubBigInt(partSize4, partSize3);
    BigInt b4 = absB.getSubBigInt(0, partSize4);

    // Evaluate polynomials at points 0, 1, -1, -2, 2, ∞ (for five points)
    BigInt p0 = a0;
    BigInt p1 = a0 + a1 + a2 + a3 + a4;
    BigInt p2 = a0 - a1 + a2 - a3 + a4;
    BigInt p3 = a0 + (BigInt("2") * a1) + (BigInt("4") * a2) + (BigInt("8") * a3) + (BigInt("16") * a4);
    BigInt p4 = a0 - (BigInt("2") * a1) + (BigInt("4") * a2) - (BigInt("8") * a3) + (BigInt("16") * a4);
    BigInt p5 = a4;

    BigInt q0 = b0;
    BigInt q1 = b0 + b1 + b2 + b3 + b4;
    BigInt q2 = b0 - b1 + b2 - b3 + b4;
    BigInt q3 = b0 + (BigInt("2") * b1) + (BigInt("4") * b2) + (BigInt("8") * b3) + (BigInt("16") * b4);
    BigInt q4 = b0 - (BigInt("2") * b1) + (BigInt("4") * b2) - (BigInt("8") * b3) + (BigInt("16") * b4);
    BigInt q5 = b4;

    // Recursive multiplications at each evaluated point
    BigInt r0 = toom5Multiply(p0, q0);
    BigInt r1 = toom5Multiply(p1, q1);
    BigInt r2 = toom5Multiply(p2, q2);
    BigInt r3 = toom5Multiply(p3, q3);
    BigInt r4 = toom5Multiply(p4, q4);
    BigInt r5 = toom5Multiply(p5, q5);

    // Interpolate and combine results
    // Interpolation involves solving a system to get final coefficients for result.
    BigInt r00 = r0;
    BigInt r05 = r5;
    BigInt r04 = (r4 - r1) / BigInt("3");
    BigInt r03 = (r3 - r2) / BigInt("3");
    BigInt r02 = (r2 - r0) / BigInt("2");
    BigInt r01 = (r1 - r3) / BigInt("2");

    // Combine coefficients to form the final result
    BigInt result = r00
        + (r01.shiftLeft(partSize1))
        + (r02.shiftLeft(2 * partSize1))
        + (r03.shiftLeft(3 * partSize1))
        + (r04.shiftLeft(4 * partSize1))
        + (r05.shiftLeft(5 * partSize1));

    // Adjust sign based on original signs of `a` and `b`
    if (isANegative != isBNegative) {
        result = -result;
    }

    return result;
}


BigInt power(BigInt a, BigInt b, const BigInt& mod) {
    BigInt result("1");
    a = a % mod;
    while (b != BigInt("0")) {
        if ((b % BigInt("2")) != BigInt("0")) {
            result = (result * a) % mod;
        }
        a = (a * a) % mod;
        b = b / BigInt("2");
    }
    return result;
}

// Function to compute the Jacobi symbol (a/n)
int jacobi(const BigInt& a, const BigInt& n) {
    if (n.getIsNegative() || n == BigInt("0") || n % BigInt("2") == BigInt("0")) return 0;

    BigInt A = a, N = n;
    int result = 1;

    if (A.getIsNegative()) {
        A = -A;
        if (N % BigInt("4") == BigInt("3")) result = -result;
    }

    while (A != BigInt("0")) {
        while (A % BigInt("2") == BigInt("0")) {
            A = A / BigInt("2");
            if (N % BigInt("8") == BigInt("3") || N % BigInt("8") == BigInt("5")) {
                result = -result;
            }
        }
        std::swap(A, N);
        if (A % BigInt("4") == BigInt("3") && N % BigInt("4") == BigInt("3")) {
            result = -result;
        }
        A = A % N;
    }
    return (N == BigInt("1")) ? result : 0;
}

// Solovay–Strassen primality test using BigInt
bool Solovay(const BigInt& n, int k = 5) {
    if (n < BigInt("2")) return false;
    if (n == BigInt("2")) return true;
    if (n % BigInt("2") == BigInt("0")) return false;

    srand(time(0));

    for (int i = 0; i < k; ++i) {
        // Generate a random BigInt a such that 2 <= a <= n - 2
        BigInt a = BigInt("2") + (BigInt(std::to_string(rand())) % (n - BigInt("3")));

        int jacobian = jacobi(a, n);  // Compute Jacobi symbol (a/n)
        if (jacobian == 0) return false;

        BigInt mod = power(a, (n - BigInt("1")) / BigInt("2"), n);

        // Compare mod with jacobian result as BigInt
        BigInt jacobianMod = (jacobian == -1) ? n - BigInt("1") : BigInt(std::to_string(jacobian));

        if (mod != jacobianMod) return false;
    }
    return true;
}

BigInt randomBigInt(const BigInt& min, const BigInt& max) {
    std::string randomStr;
    size_t len = max.getValue().size();
    for (size_t i = 0; i < len; ++i) {
        randomStr += std::to_string(rand() % 10);  // Simple random digit
    }
    BigInt randNum(randomStr);
    return randNum.isLessThan(min) ? min : randNum;
}

bool millerRabin(const BigInt& n, int k = 5) {
    if (n == BigInt("1") || n == BigInt("0")) return false;
    if (n == BigInt("2") || n == BigInt("3")) return true;
    if (n % BigInt("2") == BigInt("0")) return false;

    BigInt d = n - BigInt("1");
    int r = 0;
    while (d % BigInt("2") == BigInt("0")) {
        d /= BigInt("2");
        r++;
    }

    for (int i = 0; i < k; i++) {
        BigInt a = randomBigInt(BigInt("2"), n - BigInt("2"));
        BigInt x = power(a, d, n);  // Calculate a^d % n

        if (x == BigInt("1") || x == n - BigInt("1")) continue;

        bool found = false;
        for (int j = 0; j < r - 1; j++) {
            x = (x * x) % n;
            if (x == n - BigInt("1")) {
                found = true;
                break;
            }
        }
        if (!found) return false;
    }
    return true;
}


BigInt modularExponentiation(const BigInt& base, const BigInt& exp) {
    BigInt result("1");
    BigInt current_base = base;
    BigInt exponent = exp;

    while (exponent > BigInt("0")) {
        if (exponent % BigInt("2") == BigInt("1")) {
            result = result * current_base;
        }
        current_base = current_base * current_base;
        exponent = exponent / BigInt("2");
    }

    return result;
}

bool lucasLehmerTest(const BigInt& p) {
    if (millerRabin(p) == true) {
        return true;
    }
    BigInt two("2");
    BigInt one("1");
    if (p == one) return true;

    if (p == two) return true;

    // Compute Mersenne number M = 2^p - 1 using modular exponentiation
    BigInt M = modularExponentiation(two, p) - one;

    BigInt s("4");
    BigInt limit = p - two;
    while (limit > BigInt("0")) {
        s = ((karatsuba(s,s)) - two) % M;
        limit = limit - one;
        //std::cout << "M: " << M << std::endl;
        //std::cout << "s: " << s << std::endl;
    }

    return s == BigInt("0");
}

template<typename Func, typename... Args>
auto evaluateExecutionSpeed(Func&& func, Args&&... args) {
    // Record start time
    auto start = std::chrono::high_resolution_clock::now();

    // Execute the function and capture its return value
    auto result = std::forward<Func>(func)(std::forward<Args>(args)...);

    // Record end time
    auto end = std::chrono::high_resolution_clock::now();

    // Calculate the elapsed time in clock cycles
    auto duration = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count();

    // Return the result of the function and the duration in cycles
    return std::make_pair(result, duration);
}

int main() {
    BigInt num1("-5546748658");
    BigInt num2("-13375879053092");
    BigInt num3;
    bool y = num1 > num2;
    num3 = num1 + num2;
    BigInt num4("122333444455555666666777777788888888999999999");
    BigInt num5("999999999888888887777777666666555554444333221");
    BigInt num12("28");
    BigInt num6,num7,num8,num9,num10,num11;
    bool k = lucasLehmerTest(num12);
    BigInt den1("20");
    BigRational rational1(num5, den1);

    BigInt numer7("-5");
    BigInt denom7("5");
    BigRational rational3(numer7, denom7);
    
    BigInt numer6("-43");
    BigInt denom6("5");
    BigInt null("0");
    BigRational rational2(numer6, denom6);
    TBigRational<BigInt> trational1(denom7, denom6);
    std::cout << rational2 + rational3 << std::endl;
    std::cout << rational2 - rational3 << std::endl;
    std::cout << rational2 * rational3 << std::endl;
    std::cout << rational3 / rational2 << std::endl;
    std::cout << trational1 << std::endl;
    /*// Addition
    auto [add_result, add_time] = evaluateExecutionSpeed([](const BigInt& x, const BigInt& y) { return x + y; }, num1, num2);
    std::cout << "BigInt Addition Result: " << add_result << ", Time (ns): " << add_time << std::endl;

    // Subtraction
    auto [sub_result, sub_time] = evaluateExecutionSpeed([](const BigInt& x, const BigInt& y) { return x - y; }, num1, num2);
      std::cout << "BigInt Subtraction Result: " << sub_result << ", Time (ns): " << sub_time << std::endl;

    // Multiplication
    auto [mul_result, mul_time] = evaluateExecutionSpeed([](const BigInt& x, const BigInt& y) { return x * y; }, num1, num2);
    std::cout << "BigInt Multiplication Result: " << mul_result << ", Time (ns): " << mul_time << std::endl;

    // Division
    auto [div_result, div_time] = evaluateExecutionSpeed([](const BigInt& x, const BigInt& y) { return x / y; }, num2, num1);
    std::cout << "BigInt Division Result: " << div_result << ", Time (ns): " << div_time << std::endl;

    BigNat num3("12345678901234567890");
    BigNat num4("98765432109876543210");

    // Addition
    auto [add_result_nat, add_time_nat] = evaluateExecutionSpeed([](const BigNat& x, const BigNat& y) { return x + y; }, num3, num4);
    std::cout << "BigNat Addition Result: " << add_result_nat << ", Time (ns): " << add_time_nat << std::endl;

    // Subtraction
    auto [sub_result_nat, sub_time_nat] = evaluateExecutionSpeed([](const BigNat& x, const BigNat& y) { return x - y; }, num4, num3);
    std::cout << "BigNat Subtraction Result: " << sub_result_nat << ", Time (ns): " << sub_time_nat << std::endl;

    // Multiplication
    auto [mul_result_nat, mul_time_nat] = evaluateExecutionSpeed([](const BigNat& x, const BigNat& y) { return x * y; }, num3, num4);
    std::cout << "BigNat Multiplication Result: " << mul_result_nat << ", Time (ns): " << mul_time_nat << std::endl;

    // Division
    auto [div_result_nat, div_time_nat] = evaluateExecutionSpeed([](const BigNat& x, const BigNat& y) { return x / y; }, num4, num3);
    std::cout << "BigNat Division Result: " << div_result_nat << ", Time (ns): " << div_time_nat << std::endl;

    BigInt num5("20");
    BigInt den1("20");
    BigRational rational1(num5, den1);

    BigInt num6("-5");
    BigInt den2("5");
    BigRational rational2(num6, den2);

    // Addition
    auto [add_result_rat, add_time_rat] = evaluateExecutionSpeed([](const BigRational& x, const BigRational& y) { return x + y; }, rational1, rational2);
    std::cout << "BigRational Addition Result: " << add_result_rat << ", Time (ns): " << add_time_rat << std::endl;

    // Subtraction
    auto [sub_result_rat, sub_time_rat] = evaluateExecutionSpeed([](const BigRational& x, const BigRational& y) { return x - y; }, rational2, rational1);
    std::cout << "BigRational Subtraction Result: " << sub_result_rat << ", Time (ns): " << sub_time_rat << std::endl;

    // Multiplication
    auto [mul_result_rat, mul_time_rat] = evaluateExecutionSpeed([](const BigRational& x, const BigRational& y) { return x * y; }, rational1, rational2);
    std::cout << "BigRational Multiplication Result: " << mul_result_rat << ", Time (ns): " << mul_time_rat << std::endl;

    // Division
    auto [div_result_rat, div_time_rat] = evaluateExecutionSpeed([](const BigRational& x, const BigRational& y) { return x / y; }, rational2, rational1);
    std::cout << "BigRational Division Result: " << div_result_rat << ", Time (ns): " << div_time_rat << std::endl;

    TBigRational<int> rational3(20, 20);
    TBigRational<int> rational4(-5, 5);

    // Addition
    auto [add_result_trat, add_time_trat] = evaluateExecutionSpeed([](const TBigRational<int>& x, const TBigRational<int>& y) { return x + y; }, rational3, rational4);
    std::cout << "TBigRational Addition Result: " << add_result_trat << ", Time (ns): " << add_time_trat << std::endl;

    // Subtraction
    auto [sub_result_trat, sub_time_trat] = evaluateExecutionSpeed([](const TBigRational<int>& x, const TBigRational<int>& y) { return x - y; }, rational3, rational4);
    std::cout << "TBigRational Subtraction Result: " << sub_result_trat << ", Time (ns): " << sub_time_trat << std::endl;

    // Multiplication
    auto [mul_result_trat, mul_time_trat] = evaluateExecutionSpeed([](const TBigRational<int>& x, const TBigRational<int>& y) { return x * y; }, rational3, rational4);
    std::cout << "TBigRational Multiplication Result: " << mul_result_trat << ", Time (ns): " << mul_time_trat << std::endl;

    // Division
    auto [div_result_trat, div_time_trat] = evaluateExecutionSpeed([](const TBigRational<int>& x, const TBigRational<int>& y) { return x / y; }, rational3, rational4);
    std::cout << "TBigRational Division Result: " << div_result_trat << ", Time (ns): " << div_time_trat << std::endl;

    BigInt num7("-1234");
    BigInt num8("-1234567890123456");

    // Karatsuba Multiplication
    auto [kar_result, kar_time] = evaluateExecutionSpeed(karatsuba, num7, num8);
    std::cout << "Karatsuba Multiplication Result: " << kar_result << ", Time (ns): " << kar_time << std::endl;

    BigInt num9("1234567890123456");
    BigInt num10("1234");

    // Toom-3 Multiplication
    auto [toom3_result, toom3_time] = evaluateExecutionSpeed(toom3Multiply, num9, num10);
    std::cout << "Toom-3 Multiplication Result: " << toom3_result << ", Time (ns): " << toom3_time << std::endl;

    BigInt num11("123456");
    BigInt num12("789123");

    // Toom-5 Multiplication
    auto [toom5_result, toom5_time] = evaluateExecutionSpeed(toom5Multiply, num11, num12);
    std::cout << "Toom-5 Multiplication Result: " << toom5_result << ", Time (ns): " << toom5_time << std::endl;

    BigInt prime("1741");
    int k = 5;

    // Primality Test (isPrime)
    auto [prime_result, prime_time] = evaluateExecutionSpeed(isPrime, prime, k);
    std::cout << prime << " is " << (prime_result ? "probably prime." : "composite.") << ", Time (ns): " << prime_time << std::endl;

    BigInt a("1001");
    BigInt n("9907");

    // Jacobi Symbol Calculation
    auto [jacobi_result, jacobi_time] = evaluateExecutionSpeed(jacobi, a, n);
    std::cout << "Jacobi(" << a << "/" << n << ") = " << jacobi_result << ", Time (ns): " << jacobi_time << std::endl;

    BigInt prime2("104729");

    // Miller-Rabin Primality Test
    auto [miller_result, miller_time] = evaluateExecutionSpeed(millerRabin, prime2, k);
    std::cout << prime2 << " is " << (miller_result ? "prime." : "not prime.") << ", Time (ns): " << miller_time << std::endl;

    BigInt prime3("104729");

    // Lucas-Lehmer Primality Test
    auto [lucas_result, lucas_time] = evaluateExecutionSpeed(lucasLehmerTest, prime3);
    std::cout << prime3 << " is " << (lucas_result ? "prime." : "not prime.") << ", Time (ns): " << lucas_time << std::endl;
    */
    return 0;
}

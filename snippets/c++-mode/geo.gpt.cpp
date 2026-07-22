# -*- mode: snippet -*-
# name: geogpt
# key: geogpt
# --
#pragma once
// https://github.com/Johniel/.emacs.d/blob/master/snippets/c%2B%2B-mode/geo.gpt.cpp

#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <deque>
#include <iomanip>
#include <istream>
#include <limits>
#include <numeric>
#include <numbers>
#include <optional>
#include <ostream>
#include <random>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

namespace geometry {

using i64 = long long;
using i128 = __int128_t;
using u128 = __uint128_t;
using Real = long double;

inline constexpr Real EPS = 1e-12L;
inline constexpr Real PI = std::numbers::pi_v<Real>;
inline constexpr Real INF = std::numeric_limits<Real>::infinity();

inline int sgn(Real x) {
    return (x > EPS) - (x < -EPS);
}

inline bool eq(Real a, Real b) {
    return std::abs(a - b) <= EPS;
}

inline bool le(Real a, Real b) {
    return a <= b + EPS;
}

inline bool lt(Real a, Real b) {
    return a < b - EPS;
}

inline Real clamp_unit(Real x) {
    return std::clamp(x, -1.0L, 1.0L);
}

template <class T>
struct Point {
    T x{};
    T y{};

    constexpr Point() = default;
    constexpr Point(T x_, T y_) : x(x_), y(y_) {}

    template <class U>
    explicit constexpr operator Point<U>() const {
        return Point<U>(static_cast<U>(x), static_cast<U>(y));
    }

    constexpr Point operator+() const { return *this; }
    constexpr Point operator-() const { return Point(-x, -y); }

    constexpr Point& operator+=(const Point& rhs) {
        x += rhs.x;
        y += rhs.y;
        return *this;
    }
    constexpr Point& operator-=(const Point& rhs) {
        x -= rhs.x;
        y -= rhs.y;
        return *this;
    }
    constexpr Point& operator*=(const T& k) {
        x *= k;
        y *= k;
        return *this;
    }
    constexpr Point& operator/=(const T& k) {
        x /= k;
        y /= k;
        return *this;
    }

    friend constexpr Point operator+(Point lhs, const Point& rhs) { return lhs += rhs; }
    friend constexpr Point operator-(Point lhs, const Point& rhs) { return lhs -= rhs; }
    friend constexpr Point operator*(Point lhs, const T& k) { return lhs *= k; }
    friend constexpr Point operator*(const T& k, Point rhs) { return rhs *= k; }
    friend constexpr Point operator/(Point lhs, const T& k) { return lhs /= k; }

    friend constexpr bool operator==(const Point&, const Point&) = default;
    friend constexpr bool operator<(const Point& lhs, const Point& rhs) {
        return std::tie(lhs.x, lhs.y) < std::tie(rhs.x, rhs.y);
    }
};

template <class T>
std::istream& operator>>(std::istream& is, Point<T>& p) {
    return is >> p.x >> p.y;
}

template <class T>
std::ostream& operator<<(std::ostream& os, const Point<T>& p) {
    return os << p.x << ' ' << p.y;
}

using P = Point<Real>;
using IP = Point<i64>;

inline P to_real(const IP& p) {
    return P(static_cast<Real>(p.x), static_cast<Real>(p.y));
}

template <class T>
constexpr auto dot(const Point<T>& a, const Point<T>& b) {
    return a.x * b.x + a.y * b.y;
}

template <class T>
constexpr auto cross(const Point<T>& a, const Point<T>& b) {
    return a.x * b.y - a.y * b.x;
}

template <class T>
constexpr auto cross(const Point<T>& o, const Point<T>& a, const Point<T>& b) {
    return cross(a - o, b - o);
}

template <class T>
constexpr auto norm2(const Point<T>& p) {
    return dot(p, p);
}

inline Real length(const P& p) {
    return std::sqrt(norm2(p));
}

inline Real distance(const P& a, const P& b) {
    return length(a - b);
}

inline bool approx_equal(const P& a, const P& b) {
    return length(a - b) <= EPS;
}

template <class T>
constexpr Point<T> perp_ccw(const Point<T>& p) {
    return Point<T>(-p.y, p.x);
}

template <class T>
constexpr Point<T> perp_cw(const Point<T>& p) {
    return Point<T>(p.y, -p.x);
}

inline P unit(const P& p) {
    const Real len = length(p);
    assert(len > EPS);
    return p / len;
}

inline P rotate(const P& p, Real rad) {
    const Real c = std::cos(rad);
    const Real s = std::sin(rad);
    return P(p.x * c - p.y * s, p.x * s + p.y * c);
}

inline P rotate90(const P& p) {
    return perp_ccw(p);
}

inline Real arg(const P& p) {
    return std::atan2(p.y, p.x);
}

inline Real angle(const P& a, const P& b) {
    return std::atan2(cross(a, b), dot(a, b));
}

inline Real unsigned_angle(const P& a, const P& b) {
    return std::acos(clamp_unit(dot(a, b) / (length(a) * length(b))));
}

inline Real normalize_angle(Real a) {
    while (a <= -PI) a += 2 * PI;
    while (a > PI) a -= 2 * PI;
    return a;
}

inline i128 dot128(const IP& a, const IP& b) {
    return static_cast<i128>(a.x) * b.x + static_cast<i128>(a.y) * b.y;
}

inline i128 cross128(const IP& a, const IP& b) {
    return static_cast<i128>(a.x) * b.y - static_cast<i128>(a.y) * b.x;
}

inline i128 cross128(const IP& o, const IP& a, const IP& b) {
    return cross128(a - o, b - o);
}

inline int sgn128(i128 x) {
    return (x > 0) - (x < 0);
}

inline u128 abs_u128(i128 x) {
    if (x >= 0) return static_cast<u128>(x);
    return static_cast<u128>(-(x + 1)) + 1;
}

inline i128 gcd128(i128 a, i128 b) {
    u128 x = abs_u128(a);
    u128 y = abs_u128(b);
    while (y != 0) {
        const u128 r = x % y;
        x = y;
        y = r;
    }
    return static_cast<i128>(x);
}

inline std::string to_string(i128 x) {
    if (x == 0) return "0";
    const bool neg = x < 0;
    u128 u = abs_u128(x);
    std::string s;
    while (u > 0) {
        s.push_back(static_cast<char>('0' + u % 10));
        u /= 10;
    }
    if (neg) s.push_back('-');
    std::reverse(s.begin(), s.end());
    return s;
}

struct Fraction {
    i128 num = 0;
    i128 den = 1;

    Fraction() = default;
    Fraction(i128 n) : num(n), den(1) {}
    Fraction(i128 n, i128 d) : num(n), den(d) {
        normalize();
    }

    void normalize() {
        assert(den != 0);
        if (den < 0) {
            num = -num;
            den = -den;
        }
        const i128 g = gcd128(num, den);
        if (g != 0) {
            num /= g;
            den /= g;
        }
    }

    Real value() const {
        return static_cast<Real>(num) / static_cast<Real>(den);
    }

    friend bool operator==(const Fraction&, const Fraction&) = default;
};

inline std::ostream& operator<<(std::ostream& os, const Fraction& f) {
    os << to_string(f.num);
    if (f.den != 1) os << '/' << to_string(f.den);
    return os;
}

struct RationalPoint {
    Fraction x;
    Fraction y;

    P value() const {
        return P(x.value(), y.value());
    }
};

inline std::ostream& operator<<(std::ostream& os, const RationalPoint& p) {
    return os << p.x << ' ' << p.y;
}

struct IntLine {
    // A x + B y + C = 0
    i128 A = 0;
    i128 B = 0;
    i128 C = 0;

    IntLine() = default;
    IntLine(i128 a, i128 b, i128 c) : A(a), B(b), C(c) {
        normalize();
    }

    static IntLine through(const IP& p, const IP& q) {
        assert(p != q);
        const i128 a = static_cast<i128>(p.y) - q.y;
        const i128 b = static_cast<i128>(q.x) - p.x;
        const i128 c = -(a * p.x + b * p.y);
        return IntLine(a, b, c);
    }

    static IntLine perpendicular_bisector(const IP& p, const IP& q) {
        assert(p != q);
        const i128 a = 2 * (static_cast<i128>(q.x) - p.x);
        const i128 b = 2 * (static_cast<i128>(q.y) - p.y);
        const i128 c = static_cast<i128>(p.x) * p.x + static_cast<i128>(p.y) * p.y
                     - static_cast<i128>(q.x) * q.x - static_cast<i128>(q.y) * q.y;
        return IntLine(a, b, c);
    }

    void normalize() {
        const i128 g = gcd128(gcd128(A, B), C);
        if (g != 0) {
            A /= g;
            B /= g;
            C /= g;
        }
        if (A < 0 || (A == 0 && B < 0) || (A == 0 && B == 0 && C < 0)) {
            A = -A;
            B = -B;
            C = -C;
        }
    }

    i128 eval(const IP& p) const {
        return A * p.x + B * p.y + C;
    }

    friend bool operator==(const IntLine&, const IntLine&) = default;
};

inline std::ostream& operator<<(std::ostream& os, const IntLine& l) {
    return os << to_string(l.A) << ' ' << to_string(l.B) << ' ' << to_string(l.C);
}

enum class ExactLineRelation {
    PARALLEL,
    COINCIDENT,
    INTERSECT
};

struct ExactLineIntersection {
    ExactLineRelation relation = ExactLineRelation::PARALLEL;
    std::optional<RationalPoint> point;
};

inline ExactLineIntersection intersect(const IntLine& l, const IntLine& m) {
    const i128 det = l.A * m.B - m.A * l.B;
    if (det == 0) {
        const bool same = (l.A * m.C == m.A * l.C) && (l.B * m.C == m.B * l.C);
        return {same ? ExactLineRelation::COINCIDENT : ExactLineRelation::PARALLEL, std::nullopt};
    }
    const i128 x_num = l.B * m.C - m.B * l.C;
    const i128 y_num = l.C * m.A - m.C * l.A;
    return {ExactLineRelation::INTERSECT, RationalPoint{Fraction(x_num, det), Fraction(y_num, det)}};
}

struct Line {
    P p;
    P v;

    Line() = default;
    Line(P p_, P v_) : p(p_), v(v_) {
        assert(length(v) > EPS);
    }

    static Line through(const P& a, const P& b) {
        assert(!approx_equal(a, b));
        return Line(a, b - a);
    }

    static Line from_abc(Real A, Real B, Real C) {
        assert(std::abs(A) > EPS || std::abs(B) > EPS);
        const P v(B, -A);
        P p;
        if (std::abs(A) > std::abs(B)) p = P(-C / A, 0);
        else p = P(0, -C / B);
        return Line(p, v);
    }

    P at(Real t) const {
        return p + v * t;
    }
};

struct Segment {
    P a;
    P b;
};

struct Circle {
    P o;
    Real r = 0;
};

using Polygon = std::vector<P>;

inline int orientation(const P& a, const P& b, const P& c) {
    return sgn(cross(a, b, c));
}

inline int orientation_exact(const IP& a, const IP& b, const IP& c) {
    return sgn128(cross128(a, b, c));
}

// AOJ CGL_1_C style classification.
enum class CCW {
    CLOCKWISE = -1,
    ON_SEGMENT = 0,
    COUNTER_CLOCKWISE = 1,
    ONLINE_BACK = 2,
    ONLINE_FRONT = -2
};

inline CCW ccw(const P& a, const P& b, const P& c) {
    const P ab = b - a;
    const P ac = c - a;
    const int cr = sgn(cross(ab, ac));
    if (cr > 0) return CCW::COUNTER_CLOCKWISE;
    if (cr < 0) return CCW::CLOCKWISE;
    if (sgn(dot(ab, ac)) < 0) return CCW::ONLINE_BACK;
    if (sgn(norm2(ab) - norm2(ac)) < 0) return CCW::ONLINE_FRONT;
    return CCW::ON_SEGMENT;
}

inline bool parallel(const Line& l, const Line& m) {
    return sgn(cross(l.v, m.v)) == 0;
}

inline bool orthogonal(const Line& l, const Line& m) {
    return sgn(dot(l.v, m.v)) == 0;
}

inline bool on_line(const Line& l, const P& q) {
    return sgn(cross(l.v, q - l.p)) == 0;
}

inline bool on_segment(const Segment& s, const P& p) {
    return sgn(cross(s.b - s.a, p - s.a)) == 0 && sgn(dot(p - s.a, p - s.b)) <= 0;
}

inline bool on_segment_exact(const IP& a, const IP& b, const IP& p) {
    return cross128(a, b, p) == 0 && dot128(p - a, p - b) <= 0;
}

inline P projection(const Line& l, const P& q) {
    return l.p + l.v * (dot(q - l.p, l.v) / norm2(l.v));
}

inline P reflection(const Line& l, const P& q) {
    return 2.0L * projection(l, q) - q;
}

enum class LineRelation {
    PARALLEL,
    COINCIDENT,
    INTERSECT
};

struct LineIntersection {
    LineRelation relation = LineRelation::PARALLEL;
    std::optional<P> point;
};

inline LineIntersection intersect(const Line& l, const Line& m) {
    const Real d = cross(l.v, m.v);
    if (sgn(d) == 0) {
        if (sgn(cross(m.p - l.p, l.v)) == 0) {
            return {LineRelation::COINCIDENT, std::nullopt};
        }
        return {LineRelation::PARALLEL, std::nullopt};
    }
    const Real t = cross(m.p - l.p, m.v) / d;
    return {LineRelation::INTERSECT, l.at(t)};
}

inline bool intersects_exact(const IP& a, const IP& b, const IP& c, const IP& d) {
    const int ab_c = orientation_exact(a, b, c);
    const int ab_d = orientation_exact(a, b, d);
    const int cd_a = orientation_exact(c, d, a);
    const int cd_b = orientation_exact(c, d, b);

    if (ab_c == 0 && on_segment_exact(a, b, c)) return true;
    if (ab_d == 0 && on_segment_exact(a, b, d)) return true;
    if (cd_a == 0 && on_segment_exact(c, d, a)) return true;
    if (cd_b == 0 && on_segment_exact(c, d, b)) return true;
    return ab_c * ab_d < 0 && cd_a * cd_b < 0;
}

enum class SegmentIntersectionType {
    NONE,
    POINT,
    OVERLAP
};

struct SegmentIntersection {
    SegmentIntersectionType type = SegmentIntersectionType::NONE;
    P a{};
    P b{}; // OVERLAP のとき [a,b]。POINT のとき a == b。
};

inline SegmentIntersection intersect(const Segment& s, const Segment& t) {
    const P r = s.b - s.a;
    const P q = t.a - s.a;
    const P u = t.b - t.a;
    const Real den = cross(r, u);

    if (sgn(den) != 0) {
        const Real alpha = cross(q, u) / den;
        const Real beta = cross(q, r) / den;
        if (le(0, alpha) && le(alpha, 1) && le(0, beta) && le(beta, 1)) {
            const P p = s.a + r * alpha;
            return {SegmentIntersectionType::POINT, p, p};
        }
        return {};
    }

    if (sgn(cross(q, r)) != 0) return {};

    const Real rr = norm2(r);
    if (rr <= EPS * EPS) {
        if (on_segment(t, s.a)) return {SegmentIntersectionType::POINT, s.a, s.a};
        return {};
    }

    Real lo = dot(t.a - s.a, r) / rr;
    Real hi = dot(t.b - s.a, r) / rr;
    if (lo > hi) std::swap(lo, hi);
    lo = std::max(lo, 0.0L);
    hi = std::min(hi, 1.0L);
    if (lt(hi, lo)) return {};
    const P a = s.a + r * lo;
    const P b = s.a + r * hi;
    if (approx_equal(a, b)) return {SegmentIntersectionType::POINT, a, a};
    return {SegmentIntersectionType::OVERLAP, a, b};
}

inline Real distance(const Line& l, const P& p) {
    return std::abs(cross(l.v, p - l.p)) / length(l.v);
}

inline Real distance(const Segment& s, const P& p) {
    const P v = s.b - s.a;
    if (norm2(v) <= EPS * EPS) return distance(s.a, p);
    const Real t = dot(p - s.a, v) / norm2(v);
    if (t < 0) return distance(p, s.a);
    if (t > 1) return distance(p, s.b);
    return distance(p, s.a + v * t);
}

inline Real distance(const Segment& s, const Segment& t) {
    if (intersect(s, t).type != SegmentIntersectionType::NONE) return 0;
    return std::min({distance(s, t.a), distance(s, t.b), distance(t, s.a), distance(t, s.b)});
}

inline std::vector<P> intersect(const Line& l, const Circle& c) {
    const P h = projection(l, c.o);
    const Real d2 = norm2(h - c.o);
    const Real r2 = c.r * c.r;
    if (d2 > r2 + EPS) return {};
    if (std::abs(d2 - r2) <= EPS) return {h};
    const Real t = std::sqrt(std::max<Real>(0, r2 - d2)) / length(l.v);
    return {h - l.v * t, h + l.v * t};
}

inline std::vector<P> intersect(const Segment& s, const Circle& c) {
    if (approx_equal(s.a, s.b)) {
        if (eq(distance(s.a, c.o), c.r)) return {s.a};
        return {};
    }
    const Line l = Line::through(s.a, s.b);
    std::vector<P> ans;
    for (const P& p : intersect(l, c)) {
        if (on_segment(s, p)) ans.push_back(p);
    }
    if (ans.size() == 2 && approx_equal(ans[0], ans[1])) ans.pop_back();
    return ans;
}

inline std::vector<P> intersect(const Circle& a, const Circle& b) {
    const P dvec = b.o - a.o;
    const Real d = length(dvec);
    if (d <= EPS) return {}; // 同心円。無限個の交点もここでは空で返す。
    if (d > a.r + b.r + EPS) return {};
    if (d < std::abs(a.r - b.r) - EPS) return {};

    const Real x = (a.r * a.r - b.r * b.r + d * d) / (2 * d);
    const Real h2 = a.r * a.r - x * x;
    const P mid = a.o + dvec * (x / d);
    if (h2 <= EPS) return {mid};
    const P off = perp_ccw(dvec) * (std::sqrt(std::max<Real>(0, h2)) / d);
    return {mid - off, mid + off};
}

inline int circle_relation(const Circle& a, const Circle& b) {
    // 4: 離れている, 3: 外接, 2: 2点交差, 1: 内接, 0: 内包/同心
    const Real d = distance(a.o, b.o);
    const Real sum = a.r + b.r;
    const Real diff = std::abs(a.r - b.r);
    if (lt(sum, d)) return 4;
    if (eq(sum, d)) return 3;
    if (lt(diff, d)) return 2;
    if (eq(diff, d)) return 1;
    return 0;
}

inline Real circle_intersection_area(const Circle& a, const Circle& b) {
    const Real d = distance(a.o, b.o);
    if (d >= a.r + b.r - EPS) {
        if (d > a.r + b.r - EPS) return 0;
    }
    if (d <= std::abs(a.r - b.r) + EPS) {
        const Real r = std::min(a.r, b.r);
        return PI * r * r;
    }
    const Real alpha = 2 * std::acos(clamp_unit((a.r * a.r + d * d - b.r * b.r) / (2 * a.r * d)));
    const Real beta = 2 * std::acos(clamp_unit((b.r * b.r + d * d - a.r * a.r) / (2 * b.r * d)));
    return 0.5L * a.r * a.r * (alpha - std::sin(alpha))
         + 0.5L * b.r * b.r * (beta - std::sin(beta));
}

inline Real triangle_circle_intersection_signed_area(P a, P b, Real r) {
    // 円の中心を原点とした有向三角形 O-a-b と円の共通部分の符号付き面積。
    const P d = b - a;
    const Real A = norm2(d);
    if (A <= EPS * EPS) return 0;

    std::vector<Real> ts{0, 1};
    const Real B = 2 * dot(a, d);
    const Real C = norm2(a) - r * r;
    const Real disc = B * B - 4 * A * C;
    if (disc > EPS) {
        const Real sq = std::sqrt(disc);
        const Real t1 = (-B - sq) / (2 * A);
        const Real t2 = (-B + sq) / (2 * A);
        if (EPS < t1 && t1 < 1 - EPS) ts.push_back(t1);
        if (EPS < t2 && t2 < 1 - EPS) ts.push_back(t2);
    }
    std::sort(ts.begin(), ts.end());

    Real ans = 0;
    for (int i = 0; i + 1 < static_cast<int>(ts.size()); ++i) {
        const P p = a + d * ts[i];
        const P q = a + d * ts[i + 1];
        const P mid = a + d * ((ts[i] + ts[i + 1]) / 2);
        if (norm2(mid) <= r * r + EPS) {
            ans += cross(p, q) / 2;
        } else {
            ans += r * r * angle(p, q) / 2;
        }
    }
    return ans;
}

inline Real polygon_circle_intersection_area(const Polygon& poly, const Circle& c) {
    Real ans = 0;
    for (int i = 0, n = static_cast<int>(poly.size()); i < n; ++i) {
        ans += triangle_circle_intersection_signed_area(poly[i] - c.o, poly[(i + 1) % n] - c.o, c.r);
    }
    return std::abs(ans);
}

inline std::vector<P> tangent_points(const Circle& c, const P& p) {
    const P v = p - c.o;
    const Real d2 = norm2(v);
    const Real r2 = c.r * c.r;
    if (d2 < r2 - EPS) return {};
    if (std::abs(d2 - r2) <= EPS) return {p};
    const Real base = r2 / d2;
    const Real coef = c.r * std::sqrt(std::max<Real>(0, d2 - r2)) / d2;
    const P h = c.o + v * base;
    const P off = perp_ccw(v) * coef;
    return {h - off, h + off};
}

struct CommonTangent {
    P on_a;
    P on_b;
};

inline std::vector<CommonTangent> common_tangents(const Circle& a, const Circle& b) {
    std::vector<CommonTangent> ans;
    const P d = b.o - a.o;
    const Real z = norm2(d);
    if (z <= EPS * EPS) return ans;

    for (int side : {-1, 1}) {
        const Real r = b.r * side;
        const Real dr = a.r - r;
        Real h2 = z - dr * dr;
        if (h2 < -EPS) continue;
        h2 = std::max<Real>(0, h2);
        for (int turn : {-1, 1}) {
            if (h2 <= EPS && turn == 1) continue;
            const P v = (d * dr + perp_ccw(d) * (std::sqrt(h2) * turn)) / z;
            ans.push_back({a.o + v * a.r, b.o + v * r});
        }
    }
    return ans;
}

inline Real signed_area2(const Polygon& poly) {
    Real s = 0;
    for (int i = 0, n = static_cast<int>(poly.size()); i < n; ++i) {
        s += cross(poly[i], poly[(i + 1) % n]);
    }
    return s;
}

inline Real signed_area(const Polygon& poly) {
    return signed_area2(poly) / 2;
}

inline Real area(const Polygon& poly) {
    return std::abs(signed_area(poly));
}

inline P centroid(const Polygon& poly) {
    Real a2 = 0;
    P c(0, 0);
    for (int i = 0, n = static_cast<int>(poly.size()); i < n; ++i) {
        const P& p = poly[i];
        const P& q = poly[(i + 1) % n];
        const Real cr = cross(p, q);
        a2 += cr;
        c += (p + q) * cr;
    }
    assert(std::abs(a2) > EPS);
    return c / (3 * a2);
}

inline bool is_convex(const Polygon& poly, bool strict = false) {
    const int n = static_cast<int>(poly.size());
    if (n < 3) return false;
    int dir = 0;
    for (int i = 0; i < n; ++i) {
        const int o = orientation(poly[i], poly[(i + 1) % n], poly[(i + 2) % n]);
        if (o == 0) {
            if (strict) return false;
            continue;
        }
        if (dir == 0) dir = o;
        else if (dir != o) return false;
    }
    return dir != 0;
}

enum class Containment {
    OUT,
    ON,
    IN
};

inline Containment contains(const Polygon& poly, const P& p) {
    bool in = false;
    for (int i = 0, n = static_cast<int>(poly.size()); i < n; ++i) {
        P a = poly[i] - p;
        P b = poly[(i + 1) % n] - p;
        if (sgn(cross(a, b)) == 0 && sgn(dot(a, b)) <= 0) return Containment::ON;
        if (a.y > b.y) std::swap(a, b);
        if (a.y <= EPS && b.y > EPS && cross(a, b) > EPS) in = !in;
    }
    return in ? Containment::IN : Containment::OUT;
}

inline Polygon convex_hull(std::vector<P> pts, bool keep_collinear = false) {
    std::sort(pts.begin(), pts.end());
    pts.erase(std::unique(pts.begin(), pts.end(), [](const P& a, const P& b) {
        return approx_equal(a, b);
    }), pts.end());

    const int n = static_cast<int>(pts.size());
    if (n <= 1) return pts;

    bool all_collinear = true;
    for (int i = 2; i < n; ++i) {
        if (orientation(pts[0], pts[1], pts[i]) != 0) {
            all_collinear = false;
            break;
        }
    }
    if (all_collinear) {
        if (keep_collinear) return pts;
        return {pts.front(), pts.back()};
    }

    Polygon lower, upper;
    auto should_pop = [keep_collinear](const P& a, const P& b, const P& c) {
        const int o = orientation(a, b, c);
        return keep_collinear ? o < 0 : o <= 0;
    };

    for (const P& p : pts) {
        while (lower.size() >= 2 && should_pop(lower[lower.size() - 2], lower.back(), p)) {
            lower.pop_back();
        }
        lower.push_back(p);
    }
    for (auto it = pts.rbegin(); it != pts.rend(); ++it) {
        while (upper.size() >= 2 && should_pop(upper[upper.size() - 2], upper.back(), *it)) {
            upper.pop_back();
        }
        upper.push_back(*it);
    }
    lower.pop_back();
    upper.pop_back();
    lower.insert(lower.end(), upper.begin(), upper.end());
    return lower;
}

inline std::vector<IP> convex_hull_exact(std::vector<IP> pts, bool keep_collinear = false) {
    std::sort(pts.begin(), pts.end());
    pts.erase(std::unique(pts.begin(), pts.end()), pts.end());
    const int n = static_cast<int>(pts.size());
    if (n <= 1) return pts;

    bool all_collinear = true;
    for (int i = 2; i < n; ++i) {
        if (cross128(pts[0], pts[1], pts[i]) != 0) {
            all_collinear = false;
            break;
        }
    }
    if (all_collinear) {
        if (keep_collinear) return pts;
        return {pts.front(), pts.back()};
    }

    std::vector<IP> lower, upper;
    auto should_pop = [keep_collinear](const IP& a, const IP& b, const IP& c) {
        const i128 cr = cross128(a, b, c);
        return keep_collinear ? cr < 0 : cr <= 0;
    };
    for (const IP& p : pts) {
        while (lower.size() >= 2 && should_pop(lower[lower.size() - 2], lower.back(), p)) lower.pop_back();
        lower.push_back(p);
    }
    for (auto it = pts.rbegin(); it != pts.rend(); ++it) {
        while (upper.size() >= 2 && should_pop(upper[upper.size() - 2], upper.back(), *it)) upper.pop_back();
        upper.push_back(*it);
    }
    lower.pop_back();
    upper.pop_back();
    lower.insert(lower.end(), upper.begin(), upper.end());
    return lower;
}

inline i128 signed_area2_exact(const std::vector<IP>& poly) {
    i128 s = 0;
    for (int i = 0, n = static_cast<int>(poly.size()); i < n; ++i) {
        s += cross128(poly[i], poly[(i + 1) % n]);
    }
    return s;
}

inline i128 boundary_lattice_points(const std::vector<IP>& poly) {
    i128 ans = 0;
    for (int i = 0, n = static_cast<int>(poly.size()); i < n; ++i) {
        const IP d = poly[(i + 1) % n] - poly[i];
        ans += std::gcd(std::abs(d.x), std::abs(d.y));
    }
    return ans;
}

inline i128 interior_lattice_points(const std::vector<IP>& poly) {
    // Pick の定理: I = (|2S| - B + 2) / 2。
    const i128 twice_area = static_cast<i128>(abs_u128(signed_area2_exact(poly)));
    const i128 boundary = boundary_lattice_points(poly);
    return (twice_area - boundary + 2) / 2;
}

struct ConvexDiameter {
    Real distance = 0;
    int i = -1;
    int j = -1;
};

inline ConvexDiameter convex_diameter(const Polygon& poly) {
    const int n = static_cast<int>(poly.size());
    if (n == 0) return {};
    if (n == 1) return {0, 0, 0};
    if (n == 2) return {distance(poly[0], poly[1]), 0, 1};

    int j = 1;
    Real best2 = 0;
    int bi = 0, bj = 1;
    for (int i = 0; i < n; ++i) {
        const int ni = (i + 1) % n;
        const P edge = poly[ni] - poly[i];
        while (cross(edge, poly[(j + 1) % n] - poly[j]) > EPS) {
            j = (j + 1) % n;
        }
        for (int x : {i, ni}) {
            for (int y : {j, (j + 1) % n}) {
                const Real d2 = norm2(poly[x] - poly[y]);
                if (d2 > best2) {
                    best2 = d2;
                    bi = x;
                    bj = y;
                }
            }
        }
    }
    return {std::sqrt(best2), bi, bj};
}

inline Real minimum_width(const Polygon& poly) {
    // CCW の凸多角形。頂点数 0,1,2 では幅 0。
    const int n = static_cast<int>(poly.size());
    if (n <= 2) return 0;
    int j = 1;
    Real ans = INF;
    for (int i = 0; i < n; ++i) {
        const int ni = (i + 1) % n;
        const P edge = poly[ni] - poly[i];
        while (cross(edge, poly[(j + 1) % n] - poly[j]) > EPS) {
            j = (j + 1) % n;
        }
        ans = std::min(ans, std::abs(cross(edge, poly[j] - poly[i])) / length(edge));
    }
    return ans;
}

inline Polygon convex_cut(const Polygon& poly, const Line& l) {
    // 有向直線 l の左側 cross(l.v, p-l.p) >= 0 を残す。
    Polygon out;
    const int n = static_cast<int>(poly.size());
    for (int i = 0; i < n; ++i) {
        const P a = poly[i];
        const P b = poly[(i + 1) % n];
        const Real ca = cross(l.v, a - l.p);
        const Real cb = cross(l.v, b - l.p);
        if (ca >= -EPS) out.push_back(a);
        if (sgn(ca) * sgn(cb) < 0) {
            const Line edge = Line::through(a, b);
            const auto hit = intersect(l, edge);
            if (hit.point) out.push_back(*hit.point);
        }
    }
    return out;
}

inline Polygon convex_intersection(Polygon a, const Polygon& b) {
    if (a.empty() || b.empty()) return {};
    if (signed_area2(a) < 0) std::reverse(a.begin(), a.end());
    Polygon clip = b;
    if (signed_area2(clip) < 0) std::reverse(clip.begin(), clip.end());
    for (int i = 0, n = static_cast<int>(clip.size()); i < n && !a.empty(); ++i) {
        a = convex_cut(a, Line::through(clip[i], clip[(i + 1) % n]));
    }
    return a;
}

inline Containment contains_convex(const Polygon& poly, const P& p) {
    // CCW、同一直線上の冗長頂点なしを想定。O(log N)。
    const int n = static_cast<int>(poly.size());
    if (n == 0) return Containment::OUT;
    if (n == 1) return approx_equal(poly[0], p) ? Containment::ON : Containment::OUT;
    if (n == 2) return on_segment(Segment{poly[0], poly[1]}, p) ? Containment::ON : Containment::OUT;

    const int left = orientation(poly[0], poly[1], p);
    const int right = orientation(poly[0], poly[n - 1], p);
    if (left < 0 || right > 0) return Containment::OUT;
    if (left == 0) return on_segment(Segment{poly[0], poly[1]}, p) ? Containment::ON : Containment::OUT;
    if (right == 0) return on_segment(Segment{poly[0], poly[n - 1]}, p) ? Containment::ON : Containment::OUT;

    int lo = 1, hi = n - 1;
    while (hi - lo > 1) {
        const int mid = std::midpoint(lo, hi);
        if (orientation(poly[0], poly[mid], p) >= 0) lo = mid;
        else hi = mid;
    }
    const int o = orientation(poly[lo], poly[(lo + 1) % n], p);
    if (o < 0) return Containment::OUT;
    if (o == 0) return Containment::ON;
    return Containment::IN;
}

inline Polygon normalize_convex_polygon(Polygon p) {
    if (p.empty()) return p;
    if (signed_area2(p) < 0) std::reverse(p.begin(), p.end());

    Polygon q;
    for (const P& x : p) {
        while (q.size() >= 2 && orientation(q[q.size() - 2], q.back(), x) == 0) q.pop_back();
        q.push_back(x);
    }
    while (q.size() >= 3 && orientation(q[q.size() - 2], q.back(), q[0]) == 0) q.pop_back();
    while (q.size() >= 3 && orientation(q.back(), q[0], q[1]) == 0) q.erase(q.begin());
    if (q.empty()) return q;

    const int pos = static_cast<int>(std::min_element(q.begin(), q.end(), [](const P& a, const P& b) {
        return std::tie(a.y, a.x) < std::tie(b.y, b.x);
    }) - q.begin());
    std::rotate(q.begin(), q.begin() + pos, q.end());
    return q;
}

inline Polygon minkowski_sum(Polygon a, Polygon b) {
    a = normalize_convex_polygon(std::move(a));
    b = normalize_convex_polygon(std::move(b));
    if (a.empty() || b.empty()) return {};
    if (a.size() == 1) {
        for (P& p : b) p += a[0];
        return b;
    }
    if (b.size() == 1) {
        for (P& p : a) p += b[0];
        return a;
    }

    std::vector<P> ea, eb;
    for (int i = 0, n = static_cast<int>(a.size()); i < n; ++i) ea.push_back(a[(i + 1) % n] - a[i]);
    for (int i = 0, n = static_cast<int>(b.size()); i < n; ++i) eb.push_back(b[(i + 1) % n] - b[i]);

    Polygon out{a[0] + b[0]};
    int i = 0, j = 0;
    while (i < static_cast<int>(ea.size()) || j < static_cast<int>(eb.size())) {
        P step(0, 0);
        if (j == static_cast<int>(eb.size()) ||
            (i < static_cast<int>(ea.size()) && sgn(cross(ea[i], eb[j])) > 0)) {
            step = ea[i++];
        } else if (i == static_cast<int>(ea.size()) || sgn(cross(ea[i], eb[j])) < 0) {
            step = eb[j++];
        } else {
            step = ea[i++] + eb[j++];
        }
        out.push_back(out.back() + step);
    }
    out.pop_back();
    return normalize_convex_polygon(std::move(out));
}

struct TriangleCenters {
    std::optional<P> circumcenter;
    std::optional<P> orthocenter;
    std::optional<P> incenter;
    std::optional<Circle> incircle;
};

inline std::optional<P> circumcenter(const P& a, const P& b, const P& c) {
    const Real d = 2 * cross(b - a, c - a);
    if (std::abs(d) <= EPS) return std::nullopt;
    const Real aa = norm2(a);
    const Real bb = norm2(b);
    const Real cc = norm2(c);
    const Real ux = (aa * (b.y - c.y) + bb * (c.y - a.y) + cc * (a.y - b.y)) / d;
    const Real uy = (aa * (c.x - b.x) + bb * (a.x - c.x) + cc * (b.x - a.x)) / d;
    return P(ux, uy);
}

inline std::optional<P> orthocenter(const P& a, const P& b, const P& c) {
    const auto o = circumcenter(a, b, c);
    if (!o) return std::nullopt;
    return a + b + c - 2.0L * *o;
}

inline std::optional<P> incenter(const P& a, const P& b, const P& c) {
    const Real la = distance(b, c);
    const Real lb = distance(c, a);
    const Real lc = distance(a, b);
    const Real sum = la + lb + lc;
    if (sum <= EPS || std::abs(cross(a, b, c)) <= EPS) return std::nullopt;
    return (a * la + b * lb + c * lc) / sum;
}

inline std::optional<Circle> incircle(const P& a, const P& b, const P& c) {
    const auto center = incenter(a, b, c);
    if (!center) return std::nullopt;
    return Circle{*center, distance(Line::through(a, b), *center)};
}

inline std::array<Real, 3> barycentric(const P& a, const P& b, const P& c, const P& p) {
    const Real den = cross(b - a, c - a);
    assert(std::abs(den) > EPS);
    const Real beta = cross(p - a, c - a) / den;
    const Real gamma = cross(b - a, p - a) / den;
    const Real alpha = 1 - beta - gamma;
    return {alpha, beta, gamma};
}

inline Circle circle_from(const P& a, const P& b) {
    const P o = (a + b) / 2.0L;
    return Circle{o, distance(a, b) / 2};
}

inline Circle circle_from(const P& a, const P& b, const P& c) {
    const auto o = circumcenter(a, b, c);
    if (o) return Circle{*o, distance(*o, a)};
    const std::array<std::pair<Real, Circle>, 3> candidates{{
        {norm2(a - b), circle_from(a, b)},
        {norm2(b - c), circle_from(b, c)},
        {norm2(c - a), circle_from(c, a)}
    }};
    return std::max_element(candidates.begin(), candidates.end(), [](const auto& x, const auto& y) {
        return x.first < y.first;
    })->second;
}

inline bool contains(const Circle& c, const P& p) {
    return distance(c.o, p) <= c.r + EPS;
}

inline Circle minimum_enclosing_circle(std::vector<P> pts, std::uint64_t seed = 0x9e3779b97f4a7c15ULL) {
    if (pts.empty()) return Circle{P(0, 0), 0};
    std::mt19937_64 rng(seed);
    std::shuffle(pts.begin(), pts.end(), rng);

    Circle c{pts[0], 0};
    for (int i = 0; i < static_cast<int>(pts.size()); ++i) {
        if (contains(c, pts[i])) continue;
        c = Circle{pts[i], 0};
        for (int j = 0; j < i; ++j) {
            if (contains(c, pts[j])) continue;
            c = circle_from(pts[i], pts[j]);
            for (int k = 0; k < j; ++k) {
                if (contains(c, pts[k])) continue;
                c = circle_from(pts[i], pts[j], pts[k]);
            }
        }
    }
    return c;
}

struct ClosestPairResult {
    Real distance = INF;
    P a{};
    P b{};
};

inline ClosestPairResult closest_pair(std::vector<P> pts) {
    const int n = static_cast<int>(pts.size());
    if (n < 2) return {};
    std::sort(pts.begin(), pts.end());
    std::vector<P> tmp(n);
    Real best2 = INF;
    P best_a, best_b;

    auto rec = [&](auto&& self, int l, int r) -> void {
        if (r - l <= 1) return;
        const int m = std::midpoint(l, r);
        const Real midx = pts[m].x;
        self(self, l, m);
        self(self, m, r);

        std::merge(pts.begin() + l, pts.begin() + m, pts.begin() + m, pts.begin() + r,
                   tmp.begin(), [](const P& a, const P& b) {
                       return std::tie(a.y, a.x) < std::tie(b.y, b.x);
                   });
        std::copy(tmp.begin(), tmp.begin() + (r - l), pts.begin() + l);

        std::vector<P> strip;
        strip.reserve(r - l);
        for (int i = l; i < r; ++i) {
            const Real dx = pts[i].x - midx;
            if (dx * dx >= best2) continue;
            for (int j = static_cast<int>(strip.size()) - 1; j >= 0; --j) {
                const Real dy = pts[i].y - strip[j].y;
                if (dy * dy >= best2) break;
                const Real d2 = norm2(pts[i] - strip[j]);
                if (d2 < best2) {
                    best2 = d2;
                    best_a = pts[i];
                    best_b = strip[j];
                }
            }
            strip.push_back(pts[i]);
        }
    };

    rec(rec, 0, n);
    return {std::sqrt(best2), best_a, best_b};
}

struct HalfPlane {
    P p;
    P v;

    HalfPlane() = default;
    HalfPlane(P p_, P v_) : p(p_), v(v_) {
        assert(length(v) > EPS);
    }

    static HalfPlane through(const P& a, const P& b) {
        return HalfPlane(a, b - a);
    }

    bool outside(const P& q) const {
        return cross(v, q - p) < -EPS;
    }

    Real angle() const {
        return std::atan2(v.y, v.x);
    }
};

inline std::optional<P> intersection_point(const HalfPlane& a, const HalfPlane& b) {
    const Real d = cross(a.v, b.v);
    if (std::abs(d) <= EPS) return std::nullopt;
    const Real t = cross(b.p - a.p, b.v) / d;
    return a.p + a.v * t;
}

inline Polygon half_plane_intersection(std::vector<HalfPlane> hs) {
    // 各半平面の有向境界の左側を残す。共通部分が有界であることを想定。
    auto direction_half = [](const P& v) {
        return (v.y > 0 || (v.y == 0 && v.x >= 0)) ? 0 : 1;
    };
    std::sort(hs.begin(), hs.end(), [&](const HalfPlane& a, const HalfPlane& b) {
        const int ha = direction_half(a.v);
        const int hb = direction_half(b.v);
        if (ha != hb) return ha < hb;
        const Real cr = cross(a.v, b.v);
        if (cr != 0) return cr > 0;
        return cross(a.v, b.p - a.p) < 0;
    });

    std::vector<HalfPlane> uniq;
    for (const HalfPlane& h : hs) {
        const bool same_direction = !uniq.empty()
            && sgn(cross(uniq.back().v, h.v)) == 0
            && dot(uniq.back().v, h.v) > 0;
        if (!same_direction) {
            uniq.push_back(h);
        } else if (h.outside(uniq.back().p)) {
            uniq.back() = h;
        }
    }

    std::deque<HalfPlane> dq;
    auto bad_back = [&](const HalfPlane& h) {
        const auto p = intersection_point(dq[dq.size() - 2], dq.back());
        return !p || h.outside(*p);
    };
    auto bad_front = [&](const HalfPlane& h) {
        const auto p = intersection_point(dq[0], dq[1]);
        return !p || h.outside(*p);
    };

    for (const HalfPlane& h : uniq) {
        while (dq.size() >= 2 && bad_back(h)) dq.pop_back();
        while (dq.size() >= 2 && bad_front(h)) dq.pop_front();
        dq.push_back(h);
    }

    while (dq.size() >= 3) {
        const auto p = intersection_point(dq[dq.size() - 2], dq.back());
        if (p && !dq.front().outside(*p)) break;
        dq.pop_back();
    }
    while (dq.size() >= 3) {
        const auto p = intersection_point(dq[0], dq[1]);
        if (p && !dq.back().outside(*p)) break;
        dq.pop_front();
    }

    if (dq.size() < 3) return {};
    Polygon poly;
    for (int i = 0, n = static_cast<int>(dq.size()); i < n; ++i) {
        const auto p = intersection_point(dq[i], dq[(i + 1) % n]);
        if (!p) return {};
        poly.push_back(*p);
    }
    return poly;
}

struct PolarLess {
    P origin{0, 0};

    static int half(const P& p) {
        return (p.y > EPS || (std::abs(p.y) <= EPS && p.x >= -EPS)) ? 0 : 1;
    }

    bool operator()(const P& a, const P& b) const {
        const P x = a - origin;
        const P y = b - origin;
        const int hx = half(x);
        const int hy = half(y);
        if (hx != hy) return hx < hy;
        const int cr = sgn(cross(x, y));
        if (cr != 0) return cr > 0;
        return norm2(x) < norm2(y);
    }
};

} // namespace geometry

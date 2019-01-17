function isLeapYear(year: number): boolean {
    const divisbleBy4 = isDivisibleBy(year, 4)
    const divisbleBy100 = isDivisibleBy(year, 100)
    const divisbleBy400 = isDivisibleBy(year, 400)
    return (divisbleBy4 && !divisbleBy100) || divisbleBy400
}

const ERROR_MARGIN = 0.000000001

function isDivisibleBy(num: number, divisor: number): boolean {
    return Math.abs(num % divisor) < ERROR_MARGIN
}

export default isLeapYear

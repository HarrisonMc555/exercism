export class ResistorColor {
    private colors: string[];

    constructor(colors: string[]) {
        if (colors.length < 2) {
            throw "At least two colors need to be present";
        }
        this.colors = colors.slice(0, 2);
    }

    value = (): number => {
        let num = 0;
        for (const color of this.colors) {
            num *= 10;
            const digit = ResistorColor.colorToDigit(color);
            if (digit !== null) {
                num += digit;
            }
        }
        return num;
    }
    
    private static colorToDigit(color: string): number | null {
        const lowerColor = color.toLowerCase();
        if (lowerColor === "black") {
            return 0;
        } else if (lowerColor === "brown") {
            return 1;
        } else if (lowerColor === "red") {
            return 2;
        } else if (lowerColor === "orange") {
            return 3;
        } else if (lowerColor === "yellow") {
            return 4;
        } else if (lowerColor === "green") {
            return 5;
        } else if (lowerColor === "blue") {
            return 6;
        } else if (lowerColor === "violet") {
            return 7;
        } else if (lowerColor === "grey") {
            return 8;
        } else if (lowerColor === "white") {
            return 9;
        } else {
            throw "Invalid color " + color;
        }
    }
}


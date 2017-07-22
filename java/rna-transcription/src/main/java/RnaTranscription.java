public class RnaTranscription {
    public String transcribe(String dnaStrand) {
        StringBuilder sb = new StringBuilder(dnaStrand.length());
        for (char c : dnaStrand.toCharArray()) {
            sb.append(transcribeBase(c));
        }
        return sb.toString();
    }

    public static char transcribeBase(char base) {
        switch (base) {
        case 'G': return 'C';
        case 'C': return 'G';
        case 'T': return 'A';
        case 'A': return 'U';
        default: return '*';
        }
    }
}

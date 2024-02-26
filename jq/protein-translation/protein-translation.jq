def protein_to_codons:
  {
    "Methionine": ["AUG"],
    "Phenylalanine": ["UUU", "UUC"],
    "Leucine": ["UUA", "UUG"],
    "Serine": ["UCU", "UCC", "UCA", "UCG"],
    "Tyrosine": ["UAU", "UAC"],
    "Cysteine": ["UGU", "UGC"],
    "Tryptophan": ["UGG"],
  };

def codon_to_protein:
  protein_to_codons
  | to_entries
  | map(.key as $key | .value | map({(.): $key}))
  | add
  | add
  | .;

def stop_codons:
  ["UAA", "UAG", "UGA"];

def take_until(cond):
  def helper($acc; cond):
    if length > 0 and (first | cond) then
      helper($acc + [first]; cond)
    else
      []
    end;
  helper([]; cond);

# https://stackoverflow.com/a/51413629
def chunks($n):
 def _chunks:
   if length <= $n then . else .[0:$n] , (.[$n:]|_chunks) end;
 _chunks;

def inside_exact($collection):
  . as $element
  | $collection
  | any(. == $element);

def parse_bases:
  label $exit
  | split("")
  | chunks(3)
  | add // empty
  | if (. | inside_exact(stop_codons)) then break $exit end
  | codon_to_protein[.] // error("Invalid codon \"\(.)\"")
;

try
  [.strand | parse_bases]
catch
  ("Invalid codon" | halt_error)

use rand::prelude::*;
use std::fmt;

/* Number of students to assign */
// const NSTUDENT: usize = 25;
const NSTUDENT: usize = 27;
/* Number of sets to assign to */
const NSET: usize = 13;
/* Number of choices of set per student */
const NCHOICE: usize = 5;
/* Default rank for unclassified sets */
const UNCLASSIFIED_RANK: usize = NSET;
/* Min-max number of students per set */
const NSPS_MIN: usize = 3;
const NSPS_MAX: usize = 5;
/* Target number of students per set */
const NSPS_TARGET: usize = 4;

const A: usize = 0;
const B: usize = 1;
const C: usize = 2;
const D: usize = 3;
const E: usize = 4;
const F: usize = 5;
const G: usize = 6;
const H: usize = 7;
const I: usize = 8;
const J: usize = 9;
const K: usize = 10;
const L: usize = 11;
const M: usize = 12;

/* Return the score (penalty) for a given choice rank.
   The lower the rank, the lower the score. */
fn score_for_rank(irank: usize) -> i64 {
    return (irank * irank) as i64;
}

/* Return the score (penalty) for a given set size,
   given the number of junior and senior students. */
fn score_for_setsize(njs: usize, nss: usize) -> i64 {
    if njs == 0 || nss == 0 {
        /* Forbid either no junior or senior student in set */
        return 100000;
    }
    /* Total number of student in group */
    let ns = njs + nss;
    /* Number of student over if group is too small or too large */
    let nover;
    if ns < NSPS_MIN {
        nover = (NSPS_MIN - ns) as i64;
    } else if ns > NSPS_MAX {
        nover = (ns - NSPS_MAX) as i64;
    } else {
        nover = 0;
    }
    /* Add a penalty if a single senior student in set */
    let nssk = if nss == 1 { 10 } else { 0 };
    /* Delta to nominal group size */
    let mut delta = ns as i64 - NSPS_TARGET as i64;
    if delta < 0 {
        delta = -delta;
    }
    delta * delta * 2 + nover * nover * 1000 + nssk
}

struct Student {
    name: String,
    senior: bool,
    /* Choices of sets, indices of set, by rank */
    choices: [usize; NCHOICE],
    /* Rank in choices for each set.
       UNCLASSIFIED_RANK for set not in choices. */
    ranks: [usize; NSET],
}

struct Classroom {
    students: Vec<Student>,
}

#[derive(Eq, PartialEq, Ord, PartialOrd)]
struct SetSelection {
    /* Score for this selection, used for ordering */
    score: i64,
    /* Number of sets selected in this selection */
    nsets: usize,
    /* Bit-set of selected sets */
    selected: [bool; NSET],
}

#[derive(Debug)]
struct Allocation {
    /* Score of this allocation */
    score: i64,
    /* Index to allocated set for each student */
    alloc: [usize; NSTUDENT],
}

impl Student {
    fn new(name: &str, senior: bool, choices: [usize; NCHOICE]) -> Student {
        let mut ranks = [UNCLASSIFIED_RANK; NSET];
        for irank in 0..choices.len() {
            let iset = choices[irank];
            assert!(iset < NSET);
            ranks[iset] = irank;
        }
        Student { name: name.to_string(), senior: senior, choices: choices, ranks: ranks }
    }
    /* Find the best set according to the set selection */
    fn best_set(&self, set_selection: &SetSelection) -> usize {
        /* Select the first set in the student choice */
        for irank in 0..NCHOICE {
            let iset = self.choices[irank];
            if set_selection.selected[iset] {
                return iset;
            }
        }
        /* Otherwise pick first set selectable */
        for iset in 0..NSET {
            if set_selection.selected[iset] {
                return iset;
            }
        }
        panic!("Should not be here")
    }
}

impl fmt::Debug for Student {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Student {{ {:20} {} [",
            self.name,
            if self.senior { "senior" } else { "junior" })?;
        for iset in &self.choices {
            write!(f, "{:1} ", (iset + 'A' as usize) as u8 as char)?;
        }
        write!(f, "] ranks=[")?;
        for irank in &self.ranks {
            write!(f, "{:2} ", irank)?;
        }
        write!(f, "] }}")
    }
}

impl Classroom {
    fn new(students: Vec<Student>) -> Classroom {
        Classroom { students: students }
    }
    fn rand_choices(rng: &mut StdRng) -> [usize; NCHOICE] {
        // Random weighting for each set
        // const SET_COEFS: [u32; NSET] = [ 100, 30, 30, 30 ];
        // const SET_COEFS: [u32; NSET] = [ 100, 100, 80, 80, 80, 80, 60, 60, 60, 40, 40, 40, 40, 30, 30, 30 ];
        const SET_COEFS: [u32; NSET] = [ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 ];
        let mut ret = [0; NCHOICE];
        let mut set_indexes = [0; NSET];
        for iset in 0..set_indexes.len() {
            set_indexes[iset] = iset;
        }
        let mut picked_sets = [false; NSET];
        assert!(NCHOICE <= NSET); // Otherwise will loop forever
        for irank in 0..NCHOICE {
            loop {
                let iset = *set_indexes.choose_weighted(rng, |is| SET_COEFS[*is]).unwrap();
                if !picked_sets[iset] {
                    ret[irank] = iset;
                    picked_sets[iset] = true;
                    break;
                }
            }
        }
        ret
    }
    /* Generate a random classroom, composed of 1/3 junior, 2/3 senior. */
    fn rand(rng: &mut StdRng) -> Classroom {
        let mut students = Vec::new();
        for istd in 0..NSTUDENT {
            students.push(Student::new(
                &format!("E{:02}", istd),
                istd > NSTUDENT / 3,
                Classroom::rand_choices(rng)));
        }
        Classroom { students: students }
    }
}

impl fmt::Debug for Classroom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (istd, student) in self.students.iter().enumerate() {
            writeln!(f, "  {:2} - {:?}", istd, student)?;
        }
        write!(f, "")
    }
}

impl SetSelection {
    fn new(score: i64, nsets: usize, selected: [bool; NSET]) -> SetSelection {
        SetSelection { score: score, nsets: nsets, selected: selected }
    }
    /* Generate all different set selections, ordered by the sum of the weight of selected set */
    fn generate(class: &Classroom, nsets_min: usize, nsets_max: usize) -> Vec<SetSelection> {
        assert!(nsets_min <= NSET);
        assert!(nsets_max <= NSET);
        assert!(nsets_min <= nsets_max);
        fn generate(selection: &[bool; NSET], set_scores: &[i64; NSET], start: usize, n: usize, ret: &mut Vec<SetSelection>) {
            if n == 0 {
                let mut score: i64 = 0;
                let mut nsets: usize = 0;
                for iset in 0..selection.len() {
                    if selection[iset] {
                        score += set_scores[iset];
                        nsets += 1;
                    }
                }
                ret.push(SetSelection::new(score * 100 / nsets as i64, nsets, *selection));
                return
            }
            if start >= NSET {
                return
            }
            let mut selection2 = [false; NSET];
            selection2.clone_from_slice(selection);
            selection2[start] = true;
            generate(&selection2, set_scores, start + 1, n-1, ret);
            selection2[start] = false;
            generate(&selection2, set_scores, start + 1, n, ret);
        }
        let mut ret = Vec::new();
        let selection = [false; NSET];
        let set_scores = Self::scores(class);
        println!("Sets scores: {:?}", set_scores);
        for nsets in nsets_min..nsets_max+1 {
            generate(&selection, &set_scores, 0, nsets, &mut ret);
        }
        ret.sort();
        println!("Generated {} set selections, from {} to {} from {} sets.", ret.len(), nsets_min, nsets_max, NSET);
        ret
    }
    /* Compute score of sets from a Classroom choices */
    fn scores(class: &Classroom) -> [i64; NSET] {
        let mut ret = [0; NSET];
        for iset in 0..NSET {
            /* The score of a set is the NSPS_MAX best ranks scores */
            let mut best_ranks = Vec::new();
            for student in &class.students {
                best_ranks.push(student.ranks[iset]);
            }
            best_ranks.sort();
            best_ranks.truncate(NSPS_MAX);
            for rank in &best_ranks {
                ret[iset] += score_for_rank(*rank);
            }
        }
        ret
    }
}

impl fmt::Debug for SetSelection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut n = 0;
        write!(f, "[")?;
        for (iset, sel) in self.selected.iter().enumerate() {
            if *sel {
                write!(f, "{:1} ", (iset + 'A' as usize) as u8 as char)?;
                n += 1;
            } else {
                write!(f, "__ ")?;
            }
        }
        write!(f, "] {} sets, score {}", n, self.score)
    }
}

impl Allocation {
    fn new(classroom: &Classroom, set_selection: &SetSelection) -> Allocation {
        let mut alloc = [999; NSTUDENT];
        for istd in 0..NSTUDENT {
            alloc[istd] = classroom.students[istd].best_set(set_selection);
        }
        Allocation { score: Self::score(alloc, classroom, set_selection), alloc: alloc }
    }
    /* Compute the score of this allocation */
    fn score(alloc: [usize; NSTUDENT], classroom: &Classroom, set_selection: &SetSelection) -> i64 {
        let mut nssps = [0; NSET];
        let mut njsps = [0; NSET];
        let mut rank_score = 0;
        for istd in 0..NSTUDENT {
            let student = &classroom.students[istd];
            let iset = alloc[istd];
            if student.senior {
                nssps[iset] += 1;
            } else {
                njsps[iset] += 1;
            }
            rank_score += score_for_rank(student.ranks[iset]);
        }
        let mut set_score = 0;
        for iset in 0..NSET {
            if set_selection.selected[iset] {
                set_score += score_for_setsize(njsps[iset], nssps[iset]);
            }
        }
        rank_score + set_score
    }
    /* Move one student of this allocation from one set to another one,
       but only if the move decrease the score.
       Return true if the allocation has been modified. */
    fn move_one(&mut self, classroom: &Classroom, set_selection: &SetSelection, rng: &mut StdRng, verbose: bool) -> bool {
        let mut alloc2 = [0; NSTUDENT];
        alloc2.clone_from_slice(&self.alloc);
        let istd = rng.gen_range(0, NSTUDENT);
        let old_iset = alloc2[istd];
        let mut new_iset;
        loop {
            new_iset = rng.gen_range(0, NSET);
            if new_iset != old_iset && set_selection.selected[new_iset] {
                break;
            }
        }
        alloc2[istd] = new_iset;
        let new_score = Self::score(alloc2, classroom, set_selection);
        if new_score >= self.score {
            // Not better
            false
        } else {
            // Better
            let student = &classroom.students[istd];
            if verbose {
                println!("MOVED student {} from set {} (rank {}) to {} (rank {}), score {} > {}",
                    istd, old_iset, student.ranks[old_iset], new_iset, student.ranks[new_iset], 
                    self.score, new_score);
            }
            self.alloc.clone_from_slice(&alloc2);
            self.score = new_score;
            true
        }
    }
    /* Swap two students sets of this allocation,
       but only if the swap decrease the score.
       Return true if the allocation has been modified. */
    fn swap_two(&mut self, classroom: &Classroom, set_selection: &SetSelection, rng: &mut StdRng, verbose: bool) -> bool {
        let mut alloc2 = [0; NSTUDENT];
        alloc2.clone_from_slice(&self.alloc);
        let istd1 = rng.gen_range(0, NSTUDENT);
        let istd2 = rng.gen_range(0, NSTUDENT);
        let old_iset1 = alloc2[istd1];
        let old_iset2 = alloc2[istd2];
        alloc2[istd1] = alloc2[istd2];
        alloc2[istd2] = old_iset1;
        let new_score = Self::score(alloc2, classroom, set_selection);
        if new_score >= self.score {
            // Not better
            false
        } else {
            // Better
            if verbose {
                let student1 = &classroom.students[istd1];
                let student2 = &classroom.students[istd2];
                println!("SWAPPED student {} from set {} (rank {}) to set {} (rank {})",
                    istd1, old_iset1, student1.ranks[old_iset1], alloc2[istd1], student1.ranks[alloc2[istd1]]);
                println!("   with student {} from set {} (rank {}) to set {} (rank {}), score {} > {}",
                    istd2, old_iset2, student2.ranks[old_iset2], alloc2[istd2], student2.ranks[alloc2[istd2]],
                    self.score, new_score);
            }
            self.alloc.clone_from_slice(&alloc2);
            self.score = new_score;
            true
        }
    }
    fn debug(&self, classroom: &Classroom) {
        println!("===========================================================");
        println!("{:?}", self);
        let mut nssps = [0; NSET];
        let mut njsps = [0; NSET];
        let mut nrank = [0; NCHOICE];
        for istd in 0..NSTUDENT {
            let student = &classroom.students[istd];
            let iset = self.alloc[istd];
            if student.senior {
                nssps[iset] += 1;
            } else {
                njsps[iset] += 1;
            }
            let rank = student.ranks[iset];
            println!("  {:?} - Set {:2} (choice {:2}, score {:4})",
                student, (iset + 'A' as usize) as u8 as char, rank + 1, score_for_rank(rank));
            if rank < NCHOICE {
                nrank[rank] += 1;
            }
        }
        println!("-----------------------------------------------------------");
        for iset in 0..NSET {
            let ntot = njsps[iset] + nssps[iset];
            if ntot > 0 {
                println!("  Set {:2} - {} students ({} junior, {} senior), score={}",
                    (iset + 'A' as usize) as u8 as char, ntot, njsps[iset], nssps[iset],
                    score_for_setsize(njsps[iset], nssps[iset]));
            }
        }
        println!("-----------------------------------------------------------");
        for irank in 0..NCHOICE {
            println!("Choice {:1} - {:2} students",
                     irank + 1, nrank[irank]);
        }
        println!("===========================================================");
    }
}

/* Search for the best allocation of a given set selection,
   using depth as the search limit.
   Return the best allocation found, if any. */
fn find_best_alloc(classroom: &Classroom, set_selection: &SetSelection, rng: &mut StdRng, depth: usize) -> Option<Allocation> {
    let mut best_alloc: Option<Allocation> = None;
    /* Loop depth time, using a new base allocation as a starting point. */
    for _i in 0..depth {
        let mut alloc = Allocation::new(&classroom, &set_selection);
        let mut stuck_counter = 0;
        loop {
            let mut modified = false;
            if alloc.move_one(classroom, set_selection, rng, false) { modified = true }
            if alloc.swap_two(classroom, set_selection, rng, false) { modified = true }
            if !modified { stuck_counter += 1 } else { stuck_counter = 0 }
            /* After a while w/o any improvement, give up. */
            if stuck_counter > depth * 10 { break }
        }
        if best_alloc.is_none() || alloc.score < best_alloc.as_ref().unwrap().score {
            // println!("Found better allocation, score {}", alloc.score);
            // alloc.debug(&classroom);
            best_alloc = Some(alloc);
        }
    }
    best_alloc
}

fn main() {
    /* Deterministic random generator for Monte-Carlo */
    // Best is 1, 50 - but one choice is 4th
    // Best is 2, 53
    let mut seed = [0; 32]; seed[0] = 4;
    let mut rng: StdRng = SeedableRng::from_seed(seed);
    /* Create a classroom */
    /* let classroom1 = Classroom::new(vec![
        Student::new("Duprez Nina", true , [ D, I, L, J, F ]),
        Student::new("Allaire Naïla", true, [ G, A, K, H, C ]),
        Student::new("Suaudeau Anaïs", true,  [ B, A, L, K, C ]),
        Student::new("Rouleau Michka", false, [ I, L, F, J, D ]),
        Student::new("Rolland Marie", true, [ I, F, D, C, J ]),
        Student::new("Girard Antoine", false,  [ L, F, D, E, K ]),
        Student::new("Bethys Jodie", false, [ L, F, E, D, J ]),
        Student::new("Hacques Malou", true, [ D, E, B, F, K ]),
        Student::new("Chataigner Sören", false,  [ I, E, A, K, C ]),
        Student::new("Massons Estelle", false, [ I, G, F, B, L ]),
        Student::new("Henni Naomy", true, [ L, F, E, J, C ]),
        Student::new("Ragnaud Fybie", false,  [ E, L, D, A, F ]),
        Student::new("Foucher Célestin",  true, [ L, B, A, C, E ]),
        Student::new("Brimaud Florian", false, [ F, J, D, I, L ]),
        Student::new("Touzot Mattéo", false,  [ B, F, L, G, A ]),
        Student::new("Das Dores Gwendoline", false, [ F, J, L, E, I ]),
        Student::new("Brunel Nina", true, [ D, L, H, F, E ]),
        Student::new("Desfossé Angèle", true,  [ B, D, G, I, L ]),
        Student::new("Guérin Rudy", true, [ D, I, L, J, F ]),
        Student::new("Grellaud Camille", true, [ D, H, B, F, C ]),
        Student::new("Savagnac Kassandra", false,  [ I, J, K, L, F ]),
        Student::new("Guillou Maëva", true, [ D, A, L, B, F ]),
        Student::new("Di Nallo Manon", false, [ D, L, J, I, E ]),
        Student::new("Delval Epona", true,  [ F, C, L, E, J ]),
        Student::new("Wagner Pierre", false, [ B, L, F, I, A ]),
    ]);*/
    /* Create a classroom */
    let classroom2 = Classroom::new(vec![
        Student::new("epona delval",  true, [ C, J, I, B, F ]),
        Student::new("kassandra savagnac", false, [ J, F, D, L, A ]),
        Student::new("soren chateigner",  false,  [ F, C, I, B, J ]),
        Student::new("camille grellaud",  true, [ F, J, L, C, A ]),
        Student::new("matteo tuzo", false, [ C, F, D, J, L ]),
        Student::new("fybie ragnaud",  false,  [ C, D, J, I, A ]),
        Student::new("jodie bethys",  false, [ C, J, D, I, B ]),
        Student::new("antoine girard", false, [ I, D, F, J, B ]),
        Student::new("gwendoline das dores",  false,  [ J, D, B, L, C ]),
        Student::new("florian bremaud",  false, [ J, D, L, C, I ]),
        Student::new("nina brunel", true, [ D, J, F, A, B ]),
        Student::new("naomy henny",  true,  [ A, J, L, C, B ]),
        Student::new("estelle masson",  false, [ J, L, C, F, B ]),
        Student::new("malou hacques", true, [ J, C, F, B, I ]),
        Student::new("manon di nalo",  false,  [ J, C, F, L, I ]),
        Student::new("landric rusquet", false, [ J, L, A, B, C ]),
        Student::new("pierre wagner",  false,  [ J, L, F, D, C ]),
    ]);
    let classroom3 = Classroom::new(vec![
        Student::new("Logan",       false,  [ I,K,B,L,C ]),
        Student::new("Juliette", false, [ E,L,K,I,C ]),
        Student::new("Soren",   true, [ I,L,M,C,A ]),
        Student::new("Maïly",   false,  [ L,I,K,E,J ]),
        Student::new("Jodie",       true,  [ I,D,C,L,E ]),
        Student::new("Louane", false, [ I,A,C,E,L ]),
        Student::new("Malou",   true, [ I,K,J,G,F ]),
        Student::new("Anaëlle",   false,  [ L,K,F,J,I ]),
        Student::new("Camille",       true,  [ I,H,K,L,C ]),
        Student::new("Antoine", true, [ I,H,L,K,E ]),
        Student::new("Naomy",   true, [ I,K,L,C,E ]),
        Student::new("Epona",   true,  [ I,C,D,M,E ]),
        Student::new("Nina B",       true,  [ M,L,I,K,B ]),
        Student::new("Ethan", false, [ L,I,F,C,K ]),
        Student::new("Manon",   true, [ L,I,E,K,H ]),
        Student::new("Elérose",   false,  [ H,M,G,F,D ]),
        Student::new("Quentin",       false,  [ A,B,C,E,G ]),
        Student::new("Dorian", false, [ M,L,G,A,K ]),
        Student::new("Chloé",   false, [ C,I,L,E,J ]),
        Student::new("Flavien",   false,  [ C,G,H,K,I ]),
        Student::new("Nawel",       false,  [ E,I,H,C,L ]),
        Student::new("Maxime", false, [ C,E,I,L,H ]),
        Student::new("Jeanne",   false, [ I,C,L,H,B ]),
        Student::new("Estelle",   true,  [ I,F,D,G,K ]),
        Student::new("Rose",       false,  [ D,F,M,K,J ]),
        Student::new("Matisse", false, [ C,A,K,G,D ]),
        Student::new("Mattéo",   true, [ D,A,G,F,M ]),
    ]);
    // let classroom_rand = Classroom::rand(&mut rng);
    let classroom = classroom3;
    println!("Our classroom is:\n{:?}", &classroom);

    /* Generate all set selections */
    let nsets_min = NSTUDENT / NSPS_MAX;
    let nsets_max = NSTUDENT / NSPS_MIN;
    let mut set_selections = SetSelection::generate(&classroom, nsets_min, nsets_max);
    set_selections.truncate(1000);

    let mut depth = 1;
    let mut curr_set_selections: Vec<&SetSelection> = Vec::new();
    for set_selection in &set_selections {
        curr_set_selections.push(set_selection);
    }
    let mut best_score = 999999;
    let mut stuck_counter = 0;
    let mut best_alloc: Option<Allocation> = None;

    loop {
        println!("Scanning {} set selections, depth {}, best score {}, stuck counter {}...",
            curr_set_selections.len(),
            depth, best_score, stuck_counter);
        let mut next_set_selections: Vec<&SetSelection> = Vec::new();
        let mut next_best_score = best_score;
        for set_selection in &curr_set_selections {
            // println!("{:?}", set_selection);
            let alloc = find_best_alloc(&classroom, set_selection, &mut rng, depth);
            match &alloc {
                None => {},
                Some(dalloc) => {
                    let score = dalloc.score;
                    if score <= best_score * 2 {
                        next_set_selections.push(set_selection);
                    }
                    if score < next_best_score {
                        println!("Found better allocation, score {}", score);
                        best_alloc = alloc;
                        next_best_score = score;
                    }
                }
            }
        }
        stuck_counter = if best_score == next_best_score { stuck_counter + 1 } else { 0 };
        best_score = next_best_score;
        curr_set_selections = next_set_selections;
        depth = depth + 1;
        if stuck_counter + depth > 30 || curr_set_selections.is_empty() { break }
    }

    match best_alloc {
        None => println!("No allocation found."),
        Some(alloc) => {
            println!("Best allocation found:");
            alloc.debug(&classroom);
        }
    }
}

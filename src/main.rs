use rand::prelude::*;
use std::fmt;

/* Number of students to assign */
const NSTUDENT: usize = 30;
/* Number of sets to assign to */
const NSET: usize = 16;
/* Number of choices of set per student */
const NCHOICE: usize = 6;
/* Default rank for unclassified sets */
const UNCLASSIFIED_RANK: usize = NSET;
/* Min-max number of students per set */
const NSPS_MIN: usize = 3;
const NSPS_MAX: usize = 5;
const NSPS_TARGET: usize = 4;

fn score_for_rank(irank: usize) -> i64 {
    return (irank * irank) as i64;
}

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
    choices: [usize; NCHOICE],
    ranks: [usize; NSET],
}

#[derive(Debug)]
struct Classroom {
    students: Vec<Student>,
}

#[derive(Eq, PartialEq, Ord, PartialOrd)]
struct SetSelection {
    score: i64,
    nsets: usize,
    selected: [bool; NSET],
}

#[derive(Debug)]
struct Allocation {
    score: i64,
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
    fn rank(&self, iset: usize) -> usize {
        self.ranks[iset]
    }
    fn senior(&self) -> bool {
        self.senior
    }
    /* Find the best set according to the set selection */
    fn best_set(&self, set_selection: &SetSelection) -> usize {
        /* Select the first set in the student choice */
        for irank in 0..NCHOICE {
            let iset = self.choices[irank];
            if set_selection.is_selectable(iset) {
                return iset;
            }
        }
        /* Pick first set selectable */
        for iset in 0..NSET {
            if set_selection.is_selectable(iset) {
                return iset;
            }
        }
        panic!("Shoud not be here")
    }
}

impl fmt::Debug for Student {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Student {{ {:12} {} [",
            self.name,
            if self.senior { "senior" } else { "junior" })?;
        for iset in &self.choices {
            write!(f, "{:2} ", iset)?;
        }
        write!(f, "] }}")
    }
}

impl Classroom {
    fn new(students: Vec<Student>) -> Classroom {
        Classroom { students: students }
    }
    fn students(&self) -> &Vec<Student> {
        &self.students
    }
    fn student(&self, istd: usize) -> &Student {
        &self.students.get(istd).unwrap()
    }
    fn rand_choices(rng: &mut StdRng) -> [usize; NCHOICE] {
        // const SET_COEF: [u32; NSET] = [ 100, 80, 60, 40, 40, 30, 20, 10 ];
        // const SET_COEFS: [u32; NSET] = [ 100, 80, 80, 60, 50, 40, 40, 30, 20, 10, 10, 10 ];
        const SET_COEFS: [u32; NSET] = [ 100, 90, 80, 80, 60, 60, 50, 40, 40, 30, 20, 10, 10, 10, 5, 5 ];
        let mut ret = [0; NCHOICE];
        let mut set_indexes = [0; NSET];
        for is in 0..set_indexes.len() {
            set_indexes[is] = is;
        }
        let mut picked_sets = [false; NSET];
        assert!(NCHOICE <= NSET); // Otherwise will loop forever
        for ic in 0..NCHOICE {
            loop {
                let is = *set_indexes.choose_weighted(rng, |is| SET_COEFS[*is]).unwrap();
                if !picked_sets[is] {
                    ret[ic] = is;
                    picked_sets[is] = true;
                    break;
                }
            }
        }
        ret
    }
    /* Generate a random classroom */
    fn rand(rng: &mut StdRng) -> Classroom {
        let mut students = Vec::<Student>::new();
        for ie in 0..NSTUDENT {
            students.push(Student::new(
                &format!("E{:02}", ie),
                ie > NSTUDENT / 3,
                Classroom::rand_choices(rng)));
        }
        Classroom { students: students }
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
            for student in class.students() {
                best_ranks.push(student.rank(iset));
            }
            best_ranks.sort();
            best_ranks.truncate(NSPS_MAX);
            for rank in &best_ranks {
                ret[iset] += score_for_rank(*rank);
            }
        }
        ret
    }
    fn is_selectable(&self, iset: usize) -> bool {
        self.selected[iset]
    }
}

impl fmt::Debug for SetSelection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut n = 0;
        write!(f, "[")?;
        for (iset, sel) in self.selected.iter().enumerate() {
            if *sel {
                write!(f, "{:2} ", iset)?;
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
            alloc[istd] = classroom.student(istd).best_set(set_selection);
        }
        Allocation { score: Self::score(alloc, classroom, set_selection), alloc: alloc }
    }
    fn score(alloc: [usize; NSTUDENT], classroom: &Classroom, set_selection: &SetSelection) -> i64 {
        let mut nssps = [0; NSET];
        let mut njsps = [0; NSET];
        let mut rank_score = 0;
        for istd in 0..NSTUDENT {
            let student = classroom.student(istd);
            let iset = alloc[istd];
            if student.senior() {
                nssps[iset] += 1;
            } else {
                njsps[iset] += 1;
            }
            rank_score += score_for_rank(student.rank(iset));
        }
        let mut set_score = 0;
        for iset in 0..NSET {
            if set_selection.is_selectable(iset) {
                set_score += score_for_setsize(njsps[iset], nssps[iset]);
            }
        }
        rank_score + set_score
    }
    fn move_one(&mut self, classroom: &Classroom, set_selection: &SetSelection, rng: &mut StdRng, verbose: bool) -> bool {
        let mut alloc2 = [0; NSTUDENT];
        alloc2.clone_from_slice(&self.alloc);
        let istd = rng.gen_range(0, NSTUDENT);
        let old_iset = alloc2[istd];
        let mut new_iset;
        loop {
            new_iset = rng.gen_range(0, NSET);
            if new_iset != old_iset && set_selection.is_selectable(new_iset) {
                break;
            }
        }
        alloc2[istd] = new_iset;
        let new_score = Self::score(alloc2, classroom, set_selection);
        if new_score >= self.score {
            false
        } else {
            let student = classroom.student(istd);
            if verbose {
                println!("MOVED student {} from set {} (rank {}) to {} (rank {}), score {} > {}",
                    istd, old_iset, student.rank(old_iset), new_iset, student.rank(new_iset), 
                    self.score, new_score);
            }
            // self.debug(&classroom, &set_selection);
            self.alloc.clone_from_slice(&alloc2);
            self.score = new_score;
            true
        }
    }
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
            false
        } else {
            if verbose {
                let student1 = classroom.student(istd1);
                let student2 = classroom.student(istd2);
                println!("SWAPPED student {} from set {} (rank {}) to set {} (rank {})",
                    istd1, old_iset1, student1.rank(old_iset1), alloc2[istd1], student1.rank(alloc2[istd1]));
                println!("   with student {} from set {} (rank {}) to set {} (rank {}), score {} > {}",
                    istd2, old_iset2, student2.rank(old_iset2), alloc2[istd2], student2.rank(alloc2[istd2]),
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
        for istd in 0..NSTUDENT {
            let student = classroom.student(istd);
            let iset = self.alloc[istd];
            if student.senior() {
                nssps[iset] += 1;
            } else {
                njsps[iset] += 1;
            }
            let rank = student.rank(iset);
            println!("  {:?} - Set {:2} (choice {:2}, score {:4})",
                student, iset, rank + 1, score_for_rank(rank));
        }
        println!("-----------------------------------------------------------");
        for iset in 0..NSET {
            let ntot = njsps[iset] + nssps[iset];
            if ntot > 0 {
                println!("  Set {:2} - {} students ({} junior, {} senior), score={}",
                    iset, ntot, njsps[iset], nssps[iset],
                    score_for_setsize(njsps[iset], nssps[iset]));
            }
        }
        println!("===========================================================");
    }
}

fn find_best_alloc(classroom: &Classroom, set_selection: &SetSelection, rng: &mut StdRng, depth: usize) -> Option<Allocation> {
    let mut best_alloc: Option<Allocation> = None;
    for _i in 0..depth {
        let mut alloc = Allocation::new(&classroom, &set_selection);
        let mut stuck_counter = 0;
        loop {
            let mut modified = false;
            if alloc.move_one(classroom, set_selection, rng, false) { modified = true }
            if alloc.swap_two(classroom, set_selection, rng, false) { modified = true }
            if !modified { stuck_counter += 1 } else { stuck_counter = 0 }
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
    /* Deterministic random generator for Monte-Carlo optimization */
    let mut rng: StdRng = SeedableRng::from_seed([23, 8, 75, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
    /* Create a classroom */
    /* let classroom1 = Classroom::new(vec![
        Student::new("E1", false, [ 1, 2, 3 ]),
        Student::new("E2", false, [ 1, 2, 3 ]),
        Student::new("E3", true, [ 1, 2, 3 ]),
        Student::new("E4", true, [ 1, 2, 3 ]),
        Student::new("E5", true, [ 1, 2, 3 ]),
    ]);*/
    let classroom2 = Classroom::rand(&mut rng);
    let classroom = classroom2;
    println!("Our classroom is:");
    for (istd, student) in classroom.students().iter().enumerate() {
        println!("  {:2} - {:?}", istd, student);
    }

    /* Generate all set selections */
    let nsets_min = NSTUDENT / NSPS_MAX;
    let nsets_max = NSTUDENT / NSPS_MIN;
    let mut set_selections = SetSelection::generate(&classroom, nsets_min, nsets_max);
    set_selections.truncate(200);

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

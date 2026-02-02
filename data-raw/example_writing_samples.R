library(tibble)
library(dplyr)
library(usethis)

# 1) Twenty writing samples about "why writing assessment is difficult"
# Ranges from S01 (Very Weak) to S20 (Excellent)
example_writing_samples <- tibble::tibble(
  ID = sprintf("S%02d", 1:20),
  text = c(
    # S01: Very Weak
    "Writing assessment is hard. People write different things. It is
    confusing.",
    # S02
    "It is hard to grade writing. Some are long and some are short. I do not
    know which is best.",
    # S03
    "Assessing writing is difficult because everyone writes differently and it
    can be hard to decide what is good or bad.",
    # S04
    "Grading essays is tough work. You have to read a lot. Sometimes the
    handwriting is bad or the grammar is wrong, and that makes it hard to give
    a score.",
    # S05
    "Writing assessment is challenging because teachers must judge ideas,
    organization, grammar, and style all at once. Different raters may focus
    on different things.",
    # S06
    "It is difficult to assess writing because it is subjective. One teacher
    might like a creative style while another teacher wants a strict structure.
    This makes the scores unfair sometimes.",
    # S07
    "Writing assessment is difficult because writing is a complex skill. Raters
    must consider ideas, organization, style, and conventions, and these
    features do not always align.",
    # S08
    "A paper with strong ideas might have weak grammar, while another has
    flawless sentences but no clear argument. Deciding which one deserves a
    higher score is a major challenge in assessment.",
    # S09
    "Assessing writing is difficult because the construct is multidimensional.
    Even with detailed rubrics, raters interpret criteria differently, and their
    judgments can be influenced by fatigue or expectations.",
    # S10
    "The difficulty in writing assessment lies in consistency. Because raters
    bring their own background knowledge and preferences to the task, achieving
    high inter-rater reliability requires extensive training and calibration.",
    # S11
    "Writing assessment is difficult because we are trying to compress a rich,
    multi-dimensional performance into a single score. Raters must weigh
    content, organization, style, and mechanics, while also dealing with
    time pressure.",
    # S12
    "Evaluating writing is challenging because no rubric can fully capture what
    makes a text effective for a particular audience. Two essays might receive
    the same score for completely different reasons, obscuring the feedback
    loop.",
    # S13
    "Writing assessment is difficult because it is context-dependent. A style
    that works for a narrative is inappropriate for a report. Raters must
    constantly adjust their internal standard based on the specific purpose of
    the prompt.",
    # S14
    "The challenge of writing assessment is distinguishing between
    surface-level errors and deep structural flaws. Raters often over-penalize
    mechanical mistakes while missing more significant issues in logic or
    argumentation due to cognitive load.",
    # S15
    "Writing assessment is difficult because it sits at the intersection of
    measurement and interpretation. Raters must translate complex judgments
    about ideas, voice, and language into discrete rubric categories, often
    losing nuance in the process.",
    # S16
    "Assessing writing is inherently difficult because it requires balancing
    consistency with sensitivity. A rubric describes general qualities, but
    individual texts vary in genre and voice. Raters must decide if an
    unconventional choice is a mistake or a stylistic innovation.",
    # S17
    "Writing assessment is challenging because of the trade-off between
    validity and reliability. Highly standardized scoring protocols often strip
    away the subjective appreciation of voice and creativity, while holistic
    scoring captures the 'whole' but risks being unreliable.",
    # S18
    "The fundamental difficulty in writing assessment is cognitive complexity.
    The rater must construct a mental model of the writer's argument while
    simultaneously evaluating against specific criteria. This dual processing
    makes the task prone to bias and halo effects.",
    # S19
    "Writing assessment is difficult because it asks us to quantify something
    fundamentally qualitative. To evaluate a piece of writing, raters integrate
    judgments about content, organization, and style, while also considering
    task demands. Scores often reflect both the text and the rater's implicit
    theory of writing.",
    # S20: Excellent
    "Writing assessment is inherently problematic because it attempts to
    standardize a socially situated act. The assessment process often
    decontextualizes the writing, stripping it of its communicative purpose.
    Consequently, the score represents a construct of 'school writing' rather
    than authentic communication, creating a validity gap that simple
    psychometrics cannot resolve."
  ),
  # Simple true quality score to drive simulated pair outcomes (1 to 20)
  quality_score = 1:20
)

# 1b) One hundred simulated student writing samples on:
# "Should students be allowed to use AI to write essays in school?"
#
# Ranges from S001 (Very Weak) to S100 (Excellent).
#
# Note: These are intentionally synthetic and designed to have a clear,
# monotonic increase in overall writing quality from 1 -> 100.
prompt_ai_essays <- "Should students be allowed to use AI to write essays in school?"

example_writing_samples100 <- tibble::tibble(
  ID = sprintf("S%03d", 1:100),
  text = c(
    # S001: Very weak
    "AI should be allowed because it writes for you. School is hard.",
    # S002
    "Students should use AI for essays. It makes it faster and you get done.",
    # S003
    "I think kids should be allowed to use AI to write essays cuz it helps and
    everyone uses it.",
    # S004
    "AI should be allowed. If you dont know what to say, it can say it for you.
    That is useful.",
    # S005
    "Students should be able to use AI for essays because it gives ideas. The
    teacher still grades it.",
    # S006
    "I think students should use AI. It can fix grammar and make it sound
    better, so why not.",
    # S007
    "Yes, students should be allowed to use AI to write essays in school. AI
    can help you when you are stuck.",
    # S008
    "Students should be allowed to use AI because it helps with writing. Some
    people are bad at writing and AI can help them.",
    # S009
    "In my opinion students should be allowed to use AI to write essays. It can
    make your essay longer and nicer.",
    # S010
    "I think AI should be allowed for essays because it saves time. If it makes
    a good essay then the student still learns something.",
    # S011
    "Students should be allowed to use AI to write essays because it can help
    with spelling and grammar. It also gives suggestions.",
    # S012
    "AI can help students with essays by giving them words and sentences. This
    can make school work easier.",
    # S013
    "I think students should be allowed to use AI for essays, but only a little
    bit. If AI does everything then it is not fair.",
    # S014
    "Students should be allowed to use AI for writing because it is like a
    calculator but for words. It helps you get the answer.",
    # S015
    "Students should be allowed to use AI to write essays. AI can help with
    ideas and can check grammar so the essay is clearer.",
    # S016
    "I think students should be allowed to use AI, because it can help with
    brainstorming. But they should still write most of it themselves.",
    # S017
    "AI should be allowed in school essays because it can support students who
    struggle. However, if you copy the whole essay that is like cheating.",
    # S018
    "Students should be allowed to use AI to help with essays. It can help make
    an outline and fix mistakes, but the student should do the thinking.",
    # S019
    "Schools should let students use AI for essays in some ways. It can help
    with organization and grammar, but it should not replace the student's
    voice.",
    # S020
    "Students should be allowed to use AI when writing essays, especially for
    help with ideas and revising. But the final essay should still be their
    own work.",
    # S021
    "AI can be helpful for students who get stuck when writing. If students can
    use AI to generate ideas, they may learn how to start and continue their
    essays. Teachers can also set rules so it is not abused.",
    # S022
    "Students should be allowed to use AI to write essays because it can help
    them improve their writing. AI can suggest better words and correct
    grammar, which might teach students what sounds right.",
    # S023
    "In school, essays are meant to show thinking. AI can help with the writing
    part, but students should still plan their argument. A fair rule would be
    to allow AI for editing but not for writing whole paragraphs.",
    # S024
    "Some students have trouble turning ideas into sentences. AI can help them
    translate their thoughts into clearer writing. However, students should not
    submit an essay that AI wrote without understanding it.",
    # S025
    "If schools ban AI completely, students may use it secretly. Instead,
    teachers could teach students how to use AI responsibly, such as asking for
    feedback and then revising the draft themselves.",
    # S026
    "Allowing AI for essays could support learning if it is used as a tutor.
    For example, a student could ask for an outline, then write the essay and
    compare it to suggested improvements.",
    # S027
    "Students should be allowed to use AI for certain parts of the writing
    process, like brainstorming and proofreading. But if AI writes the whole
    essay, the assignment no longer measures the student's skills.",
    # S028
    "AI can help students by giving feedback quickly. It can point out unclear
    sentences or suggest transitions. This can make revision more effective,
    especially when teachers have limited time.",
    # S029
    "A complete ban on AI is unrealistic. The better question is what kind of
    help is acceptable. Using AI to fix grammar seems similar to using spell
    check, while using it to generate the entire essay seems dishonest.",
    # S030
    "Students should be allowed to use AI for essays if they are transparent.
    Teachers could require students to describe how they used AI and what they
    changed, so the focus stays on learning.",
    # S031
    "There are benefits to allowing AI: it can help students organize ideas and
    write more clearly. But there are also risks: students might rely on it too
    much and stop practicing. A policy should encourage support, not
    replacement.",
    # S032
    "If AI is allowed, schools should define boundaries. Students could use AI
    to brainstorm claims, find counterarguments, or edit sentences, but they
    should not submit AI-generated work as if it were their own writing.",
    # S033
    "AI writing tools may help students who struggle with language. For
    instance, English learners could use AI to rephrase sentences and learn new
    vocabulary. Still, the student should provide the main ideas and evidence.",
    # S034
    "The purpose of essays is to practice thinking and communication. AI can be
    a helpful coach by suggesting structure and pointing out weaknesses. Yet,
    if students hand over the task to AI, they lose the practice that makes
    writing improve.",
    # S035
    "Students should be allowed to use AI to support essay writing the way they
    might use a tutor. The key is that the student remains responsible for the
    argument, evidence, and final choices, instead of copying the output.",
    # S036
    "An effective policy could allow AI for prewriting and revision but forbid
    it for final drafting. That approach recognizes that tools can help
    learning, while still requiring students to produce their own sentences in
    the final submission.",
    # S037
    "If schools forbid AI, they may spend more time policing than teaching.
    Allowing it with guidelines could turn AI into a lesson about ethics,
    citation, and the difference between assistance and authorship.",
    # S038
    "One concern is fairness. Some students have access to better tools or
    faster internet. If AI is allowed, schools should consider providing equal
    access or using school-approved tools so the playing field is not uneven.",
    # S039
    "Another concern is accuracy. AI sometimes makes confident mistakes.
    Students who rely on it may include false information. Teachers could
    require evidence from class readings and teach students to verify claims.",
    # S040
    "Students should be allowed to use AI in limited ways, because it can
    support revision and learning. At the same time, schools must protect the
    goal of writing assignments: developing a student's ability to think,
    argue, and communicate clearly.",
    # S041
    "Allowing AI can reduce barriers for students with learning differences.
    Tools that suggest wording or organize ideas could make writing less
    overwhelming. However, students still need to practice composing and
    revising to build skill over time.",
    # S042
    "If AI is used, transparency should be required. Students could note what
    they asked the tool and what they changed afterward. This makes AI support
    visible and keeps accountability on the student.",
    # S043
    "A balanced approach is to permit AI for brainstorming and editing, but
    require original drafting. Teachers can also include in-class writing or
    oral explanations so students demonstrate they understand their own
    arguments.",
    # S044
    "The strongest argument against AI-written essays is that students will not
    practice writing. Writing is like a muscle: it grows through use. If AI
    does the work, students may improve less, even if the final product looks
    polished.",
    # S045
    "On the other hand, AI can act like feedback that teachers cannot always
    provide quickly. If students use AI to identify weak transitions or vague
    claims, then revise thoughtfully, it may strengthen both the essay and the
    student's skills.",
    # S046
    "Teachers could design assignments that emphasize process rather than only
    product. For example, students could submit a thesis statement, an outline,
    and two drafts. AI could be allowed for feedback, but the student's own
    drafts would show growth.",
    # S047
    "Schools also need to teach digital ethics. Just as students learn when to
    cite sources, they should learn how to disclose AI assistance. A clear
    policy would reduce confusion and discourage secret use.",
    # S048
    "A total ban might punish students who use AI responsibly, such as to check
    grammar. But a no-limits policy might encourage full outsourcing. The best
    policy sits in the middle: allow specific uses, require disclosure, and
    design tasks that value thinking.",
    # S049
    "Students should be allowed to use AI, but not as a replacement for
    learning. A useful rule is: AI may suggest, but the student must decide.
    If the student cannot explain the essay's argument, then the tool has
    replaced the student.",
    # S050
    "To keep learning central, teachers can require students to connect essays
    to class discussions, readings, or personal experiences. That makes it
    harder for generic AI output to succeed and encourages students to bring
    their own knowledge into the writing.",
    # S051
    "Allowing AI also prepares students for the real world, where tools are
    common. Many jobs already involve drafting and editing with technology.
    School can teach students how to use AI critically instead of blindly.",
    # S052
    "However, we should not pretend AI is neutral. The tool may reflect biases
    in its training data. Students must learn to question suggestions, check
    facts, and consider whether a proposed phrasing is fair and accurate.",
    # S053
    "In practice, teachers might allow AI to generate an outline or suggest
    counterarguments, but require students to write their own paragraphs. A
    short reflection at the end could ask students what AI suggested and what
    they changed, reinforcing metacognition.",
    # S054
    "When essays are used for assessment, the rules may need to be stricter
    than when essays are used for practice. For a high-stakes grade, teachers
    might limit AI to proofreading. For a low-stakes draft, AI feedback could
    be encouraged.",
    # S055
    "One of the clearest risks is dependency. If students always ask AI for a
    thesis statement, they may never learn how to form one. Teachers can
    address this by requiring students to attempt the task first, then use AI
    for suggestions, and finally explain their revisions.",
    # S056
    "Schools should also consider privacy. Some AI tools store user prompts or
    text. Students may unintentionally share personal information. If AI is
    allowed, districts should approve tools that protect data and explain what
    is safe to share.",
    # S057
    "A thoughtful policy separates three actions: generating ideas, composing
    sentences, and submitting work. Using AI to brainstorm topics can be
    acceptable; using it to write a final draft can be unacceptable; submitting
    it without disclosure is academic dishonesty.",
    # S058
    "Rather than asking whether students may use AI, we should ask what skill
    the assignment targets. If the skill is argumentation, AI could help with
    grammar. If the skill is writing style, AI drafting undermines the goal. A
    skill-based policy is clearer than a blanket rule.",
    # S059
    "Teachers can also redesign prompts. If assignments require evidence from
    specific class texts, citations, or in-class activities, then students must
    engage with the material. AI can help polish writing, but it cannot replace
    engagement with course content.",
    # S060
    "Students should be allowed to use AI as a tool, but schools need rules
    that protect learning and integrity. A sensible approach is to allow AI
    support for brainstorming and revision, while requiring students to draft
    and defend their own ideas.",
    # S061
    "A workable classroom guideline could be: (1) Write a first draft without
    AI, (2) use AI to get feedback or alternative phrasing, (3) revise and keep
    track of what changed. This sequence maintains practice while still using
    the tool as a coach.",
    # S062
    "Disclosure matters because it makes the learning process visible. If a
    student used AI to rewrite a paragraph, the teacher should know. That does
    not mean automatic punishment; it means the teacher can respond with
    targeted instruction and expectations.",
    # S063
    "Some students will try to use AI to avoid work. Schools can reduce this by
    including checkpoints like outlines, annotated sources, and brief oral
    conferences. These steps reward genuine thinking and make outsourcing less
    effective.",
    # S064
    "Allowing AI can also support accessibility. Students with disabilities may
    benefit from tools that help them organize, dictate, or revise text. If AI
    is banned, these students might lose support. Any policy should account for
    accommodations and inclusion.",
    # S065
    "At the same time, writing fluency matters. Students need opportunities to
    struggle productively, to find words, and to revise. If AI immediately
    supplies a polished paragraph, the student may skip the cognitive work that
    builds long-term skill.",
    # S066
    "A nuanced policy can allow AI for surface-level edits, such as grammar,
    punctuation, and clarity, while restricting it for core intellectual work,
    such as developing claims and selecting evidence. This preserves the
    purpose of essay writing as thinking on paper.",
    # S067
    "Teachers can explicitly teach 'AI literacy': how to prompt for feedback,
    how to evaluate suggestions, and how to cite AI assistance. This is similar
    to teaching students how to use research databases responsibly, rather than
    banning the internet.",
    # S068
    "Equity should guide implementation. If AI is permitted, school-provided
    access reduces the advantage of students who can afford premium tools. A
    school-approved platform can also ensure consistent privacy protections and
    reduce the temptation to use risky sites.",
    # S069
    "Assessment design also matters. If the goal is to assess writing skill, in
    class writing or supervised drafting may be needed. If the goal is to
    assess reasoning, students could use AI for editing but must cite sources
    and explain their argument in discussion or a short oral defense.",
    # S070
    "Ultimately, AI is neither purely harmful nor purely helpful. It depends on
    how it is used. Schools should allow AI in ways that support learning,
    require transparency, and still give students repeated practice producing
    their own writing.",
    # S071
    "A helpful distinction is between 'support' and 'substitution'. Support
    includes brainstorming questions, outlining, or identifying unclear
    sentences. Substitution occurs when AI produces the argument and language
    that the student submits. Policies should permit the former and restrict
    the latter.",
    # S072
    "If we treat AI as a calculator for language, we risk misunderstanding what
    writing teaches. Writing is not just transcription; it is a way of
    developing ideas. Therefore, students should not outsource drafting, but
    they can use AI to improve clarity and mechanics once the ideas are theirs.",
    # S073
    "The temptation to ban AI often comes from fear of cheating, but bans are
    hard to enforce and can create adversarial classrooms. A better approach is
    to redesign assignments and build norms of disclosure, so the teacher's role
    remains instruction rather than surveillance.",
    # S074
    "Schools can combine AI policies with pedagogy: require annotated outlines,
    ask students to cite passages from course texts, and include reflective
    writing about choices made during revision. These steps not only reduce
    misuse, they also deepen learning.",
    # S075
    "Because AI can hallucinate information, students must learn verification.
    Teachers can require that factual claims be supported by credible sources.
    If a student uses AI to generate ideas, they still need to confirm evidence
    through readings, research, and classroom discussion.",
    # S076
    "Privacy, bias, and authorship are not side issues; they are central.
    Students should learn that entering personal data into an AI tool has
    consequences, and that AI suggestions may carry stereotypes. Teaching these
    issues makes AI use an opportunity for critical digital citizenship.",
    # S077
    "A clear disclosure format could help. For example: 'AI use: I asked for
    three counterarguments and for feedback on clarity. I did not copy any
    generated paragraphs.' Standard language reduces ambiguity and normalizes
    honesty.",
    # S078
    "Teachers can also provide 'AI-allowed' and 'AI-free' tasks. Some essays
    might explicitly permit AI for editing, while other assignments (like timed
    writes) measure independent writing. This aligns tool use with instructional
    goals and gives students practice in both contexts.",
    # S079
    "In addition, students can be taught to use AI for self-assessment. They
    might ask the tool to identify weak evidence, vague claims, or missing
    transitions, and then revise. Used this way, AI supports the revision cycle
    without replacing the student's authorship.",
    # S080
    "Schools should allow AI with guardrails: disclosure, limits on drafting,
    and assignment designs that emphasize process. This approach recognizes
    reality while protecting the core purpose of essay writing: learning to
    reason, communicate, and revise.",
    # S081
    "A policy that focuses only on prohibition misses the opportunity to teach
    responsible use. Students will encounter AI outside of school. Instruction
    in how to use AI ethically, verify outputs, and retain ownership of ideas
    may be more valuable than attempting to eliminate the tool.",
    # S082
    "We should also consider motivation. If essays feel like busywork, students
    will outsource them. If prompts invite genuine thinking and connect to
    students' lives, the incentive to copy decreases. Better assignment design
    is an indirect but powerful response to AI misuse.",
    # S083
    "From an equity perspective, AI can either widen or narrow gaps. It may
    help students who lack academic language by suggesting clearer phrasing,
    yet it may advantage students with better access and prompting skills.
    Schools should address this by teaching prompting as a skill and providing
    equitable access.",
    # S084
    "Integrity is best supported by clarity. Students need to know what counts
    as acceptable help. A rubric could include a category for 'process
    evidence', rewarding planning, drafting, and revision artifacts. That shifts
    attention from catching cheaters to supporting learning behaviors.",
    # S085
    "Another option is to treat AI like a cited source. If a student uses AI to
    generate an outline or rephrase a sentence, they should disclose it the way
    they would disclose a human tutor. This reinforces the concept of academic
    honesty as transparency about contributions.",
    # S086
    "In evaluating AI policies, we should ask: What do we want students to be
    able to do after school? In many professions, writing involves drafting,
    editing, collaboration, and tool use. Teaching students to work with AI
    critically can align school writing with authentic writing practices.",
    # S087
    "Nevertheless, foundational practice matters. Younger students may need
    more restrictions because they are still learning sentence construction and
    paragraph development. Older students might be permitted more AI support,
    especially for revision and style, as long as they can demonstrate
    independent competence.",
    # S088
    "A tiered policy could reflect developmental goals. For instance, middle
    school might allow AI only for grammar checks, while high school might allow
    AI for outlining and feedback, coupled with disclosure and teacher-designed
    checkpoints. Tiered policies are more realistic than one rule for all.",
    # S089
    "Teachers also need support. Without shared expectations, one teacher may
    permit AI and another may punish it, creating confusion. School-wide
    guidelines, professional development, and shared language can make AI use
    consistent and fair.",
    # S090
    "Ultimately, the key is to preserve authorship. Students should generate
    claims, choose evidence, and make rhetorical decisions. AI can be permitted
    to assist with mechanics and to offer feedback, but the student's reasoning
    must remain central and assessable.",
    # S091
    "A strong policy could combine three elements: access (so students are not
    advantaged by wealth), transparency (so teachers know what tools were used),
    and pedagogy (so assignments measure thinking through drafts, reflections,
    and discussion). This combination addresses both fairness and learning.",
    # S092
    "When we frame AI as a threat, we may miss its instructional potential. AI
    feedback can be imperfect, but it can prompt students to consider clarity,
    coherence, and tone. With teacher guidance, students can learn to critique
    suggestions and make intentional revisions.",
    # S093
    "We should also name the limitations: AI can mimic style without genuine
    understanding, it can fabricate citations, and it can produce bland
    generalities. Teaching students to recognize these weaknesses turns AI use
    into an exercise in critical reading as well as writing.",
    # S094
    "A classroom practice that supports integrity is the 'process portfolio':
    students submit brainstorming notes, outline, drafts, peer feedback, and a
    final reflection. AI may be used for feedback, but the portfolio shows the
    student's decision-making over time, making authorship difficult to fake.",
    # S095
    "In addition, teachers can incorporate oral components. Short conferences,
    presentations, or Q-and-A sessions require students to articulate their
    reasoning. If AI assisted with wording, students can still demonstrate
    comprehension and ownership of the argument.",
    # S096
    "Rather than asking whether AI should be allowed, we can ask how writing
    instruction should evolve. If AI can draft generic paragraphs, then school
    essays should emphasize analysis, personal synthesis, and evidence-based
    reasoning. AI becomes a tool in a richer learning environment, not a way to
    avoid learning.",
    # S097
    "A well-designed policy acknowledges that writing is both skill and
    practice. Students need repeated opportunities to draft on their own, but
    they also need to learn modern revision workflows. Allow AI where it
    supports revision, require disclosure, and design assessments that capture
    student thinking.",
    # S098
    "In my view, schools should permit AI with explicit boundaries: no
    AI-generated final drafts, disclosure of assistance, and structured writing
    processes that include drafts and reflections. This approach protects
    academic honesty while preparing students for real-world writing with tools.",
    # S099
    "AI use in school writing should be treated as an ethical literacy issue.
    Students must learn when tool use becomes misrepresentation, how to verify
    claims, and how to retain voice and agency. With clear guidelines, AI can
    enhance learning rather than replace it.",
    # S100: Excellent
    "Students should be allowed to use AI in school writing only within a
    framework that preserves the educational purpose of essays. Essays are not
    merely products; they are a process for developing ideas, weighing evidence,
    and making rhetorical choices. If AI replaces that process by generating a
    finished draft, the student loses the practice and the teacher loses valid
    evidence of learning.

    At the same time, a blanket ban is both unrealistic and educationally
    shallow. Students already write with tools: spell checkers, grammar
    suggestions, and online sources. The question is not whether tools exist,
    but whether students can use them responsibly. AI can function as a tutor
    when it is used to brainstorm questions, identify unclear passages, suggest
    alternative phrasing, or surface counterarguments that a student must then
    evaluate.

    A coherent policy should therefore distinguish assistance from authorship.
    Schools can allow AI for feedback and revision while requiring students to
    generate claims, select evidence, and write the final prose themselves.
    Transparency should be mandatory: students disclose prompts and describe
    what they accepted or rejected. Finally, assignments should be designed to
    emphasize process through drafts, reflections, and occasional in-class
    writing, so learning remains visible. With these guardrails, AI becomes a
    tool for learning rather than a shortcut around it."
  ),
  quality_score = 1:100,
  prompt = prompt_ai_essays
)

# 2) Complete paired comparison table implied by quality_score
# With 20 samples, this generates 190 pairs (20 * 19 / 2)
pairs_matrix <- utils::combn(example_writing_samples$ID, 2)

set.seed(1)

example_writing_pairs <- tibble::tibble(
  ID1 = pairs_matrix[1, ],
  ID2 = pairs_matrix[2, ]
) |>
  dplyr::left_join(
    example_writing_samples |>
      dplyr::select(ID, q1 = quality_score),
    by = c("ID1" = "ID")
  ) |>
  dplyr::left_join(
    example_writing_samples |>
      dplyr::select(ID, q2 = quality_score),
    by = c("ID2" = "ID")
  ) |>
  dplyr::mutate(
    # Reproducible stochastic outcomes: higher quality tends to win,
    # but not deterministically (avoids separation / convergence warnings).
    #
    # Tune `scale`:
    #   larger scale -> closer to 50/50 (more noise)
    #   smaller scale -> more deterministic (less noise)
    #
    # With scale = 2, a gap of 2 points gives p ~ 0.73, gap of 5 gives p ~ 0.92.
    .gap = q2 - q1,
    .scale = 2,
    .eta = .gap / .scale,
    .p_id2_wins = stats::plogis(.eta),
    better_id = dplyr::if_else(stats::runif(dplyr::n()) < .p_id2_wins, ID2, ID1)
  ) |>
  dplyr::select(ID1, ID2, better_id)


# 3) Example OpenAI Batch output lines (JSONL format)
# Keeping a small representative sample (3 lines) to demonstrate structure.
# Generating 190 fake JSON lines would bloat the package data unnecessarily.
example_openai_batch_output <- c(
  # Line 1: SAMPLE_1 is better (S01 vs S02 - technically S02 is better in our
  # new logic, but this dataset is for structural testing of parsers,
  # not strict truth alignment).
  '{"id": "batch_req_aaa111", "custom_id": "EXP_S01_vs_S02", "response":
  {"status_code": 200, "request_id": "req_111aaa", "body":
  {"id": "chatcmpl-111aaa", "object": "chat.completion", "created":
  1753322001, "model": "o3-2025-04-16", "choices": [{"index": 0, "message":
  {"role": "assistant", "content": "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>",
  "refusal": null, "annotations": []}, "finish_reason": "stop"}], "usage":
  {"prompt_tokens": 440, "completion_tokens": 95, "total_tokens": 535,
  "prompt_tokens_details": {"cached_tokens": 0, "audio_tokens": 0},
  "completion_tokens_details": {"reasoning_tokens": 64, "audio_tokens": 0,
  "accepted_prediction_tokens": 0, "rejected_prediction_tokens": 0}},
  "system_fingerprint": null}}, "error": null}',
  # Line 2: SAMPLE_2 is better
  '{"id": "batch_req_bbb222", "custom_id": "EXP_S01_vs_S03", "response":
  {"status_code": 200, "request_id": "req_222bbb", "body":
  {"id": "chatcmpl-222bbb", "object": "chat.completion", "created": 1753322002,
  "model": "o3-2025-04-16", "choices": [{"index": 0, "message": {"role":
  "assistant", "content": "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>", "refusal":
  null, "annotations": []}, "finish_reason": "stop"}], "usage": {"prompt_tokens"
  : 452, "completion_tokens": 90, "total_tokens": 542, "prompt_tokens_details":
  {"cached_tokens": 0, "audio_tokens": 0}, "completion_tokens_details":
  {"reasoning_tokens": 60, "audio_tokens": 0, "accepted_prediction_tokens": 0,
  "rejected_prediction_tokens": 0}}, "system_fingerprint": null}}, "error":
  null}',
  # Line 3: an error case (status_code null, error populated)
  '{"id": "batch_req_ccc333", "custom_id": "EXP_S02_vs_S03", "response":
  {"status_code": null, "request_id": "", "body": null}, "error": {"code":
  "rate_limit_exceeded", "message": "Request was not processed in batch due
  to rate limiting."}}'
)

# Save datasets into the package
usethis::use_data(example_writing_samples, overwrite = TRUE)
usethis::use_data(example_writing_pairs, overwrite = TRUE)
usethis::use_data(example_openai_batch_output, overwrite = TRUE)
usethis::use_data(example_writing_samples100, overwrite = TRUE)

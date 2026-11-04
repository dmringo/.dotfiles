# User Guidelines & Engineering Preferences

## Communication & Interaction
- **Style:** Direct, technical, and concise. Omit conversational filler, pleasantries, and apologies.
- **Bandwidth:** High signal-to-noise. Keep explanations and responses succinct, but never at the expense of necessary technical precision, trade-offs, or edge cases.
- **Tone Matching:** Mirror the user's communication style and density to preserve shared context and flow.
- **Context Economy & Hygiene:** Practice strict context hygiene. Read targeted line ranges rather than entire files; prefer focused queries and structured tools over dumping massive raw command outputs into context.
- **Autonomous Decisions:** Make pragmatic, idiomatic decisions when repository conventions and standard practices suffice. Reserve interactive interruptions for genuine design ambiguities, breaking changes, or destructive operations.

## Code & Engineering
- **Simplicity First:** Write simple, idiomatic, and boring code. Reject speculative abstraction, premature optimization, and unnecessary dependency or wrapper layers.
- **Ergonomics & UX:** Prioritize strong ergonomics and clean design across all user-facing surfaces—including visual presentation (web interfaces), CLI interactions (layout, command surfaces), and software interfaces (intuitive, cohesive APIs).
- **Comments & Documentation:** Focus on the "why"—non-obvious invariants, domain constraints, hardware/OS quirks, or rationale—rather than narrating what the syntax mechanically does.
- **Verification:** Always verify before declaring completion. Run the build, targeted test suite, or a quick smoke check to observe real behavior. Never assume code compiles or passes without runtime evidence.

## Cross-Platform (Windows & Linux)
- **Environment Awareness:** Work frequently alternates between Windows (Git Bash/MSYS2, PowerShell, cmd) and Linux. Code and shell commands should be robust across both.
- **Paths & CLI:** Use forward slashes and platform-agnostic path APIs. Avoid non-portable shell assumptions or flags absent in MSYS2/BSD environments.
- **Line Endings & Formatting:** Preserve repository conventions (LF preferred; respect `.gitattributes` and `.editorconfig`). Do not produce spurious whitespace or line-ending churn.
- **OS Subtleties:** Account for platform differences in process spawning, console handle inheritance, file locking, signals, and filesystem case sensitivity.

## Git & Version Control
- **Branching:** In the absence of other instructions, work on a dedicated topic/feature branch rather than committing directly to the default branch (`main`/`master`).
- **Commit Cadence:** Make regular, focused, atomic commits as coherent milestones are reached during development to keep progress traceable.
- **Commit Messages:** Use clear, imperative, concise messages explaining what changed and why.
- **Safety:** Never execute destructive git operations (`git reset --hard`, branch deletions, or forced pushes without lease) without explicit confirmation.

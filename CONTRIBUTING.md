# Contributing to Fluxer

Thanks for contributing. This document explains how we work so your changes can land smoothly and nobody wastes time on work we can't merge.

## Quick rules (please read)

### 1) All PRs must target `canary`

`canary` is our trunk branch. Open all pull requests against `canary`. PRs targeting other branches will be closed or retargeted.

### 2) All PRs must include a short description

Every PR must include a short description covering:

- what changed
- why it changed
- anything reviewers should pay attention to

A few bullets is fine.

### 3) Open an issue before submitting a PR

We strongly prefer that every PR addresses an existing issue. If one doesn't exist yet, open one describing the problem or improvement and your proposed approach. This gives maintainers a chance to weigh in on direction before you invest time, and avoids the mutual displeasure of:

- you doing significant work, and
- us having to reject or postpone the change because it doesn't align with current goals, or because we aren't ready to maintain what it introduces

For small, obvious fixes (typos, broken links, trivial one-liners) you can skip the issue and go straight to a PR.

Ways to coordinate on larger work:

- open an issue describing the problem and your proposed approach
- open a draft PR early to confirm direction
- discuss with a maintainer in any channel you already share

If you're unsure whether something needs an issue first, it probably does.

### 4) Understand the code you submit

You must have sufficient understanding of every change in your PR to explain it and defend it during review. You don't need to write an essay, but you should be able to give a short summary of what the patch does and why it's correct.

**LLM-assisted contributions.** You're welcome to use LLMs as a tool for automating mechanical work. We don't ask you to disclose this, since we assume you're acting in good faith: you're the one who signs off on the patch you submit in your own name, and you have the technical understanding to verify that it's accurate.

That said, don't use LLMs on areas of the codebase you don't understand well enough to verify the output. If part of your change touches code you aren't confident reviewing yourself, say so in the issue you opened beforehand and defer that work to someone else. The maintainers will be happy to help.

## Workflow

1. Fork the repo (or create a branch if you have access).
2. Create a feature branch from `canary`.
3. Make changes.
4. Open a PR into `canary` with a short description.
5. Address review feedback and CI results.
6. We squash-merge approved PRs into `canary`.

We strongly prefer small, focused PRs that are easy to review.

### Commit style and history

We squash-merge PRs, so the PR title becomes the single commit message on `canary`. For that reason:

- PR titles must follow Conventional Commits.
- Individual commits inside the PR don't need to follow Conventional Commits.

If you like to commit in small increments, feel free. If you prefer a tidier PR history, force-pushes are welcome (for example, to squash or reorder commits before review). Just avoid rewriting history in a way that makes it hard for reviewers to follow along.

## Conventional Commits (required for PR titles)

Because the PR title becomes the squash commit message, we require Conventional Commits for PR titles.

We prefer type/subject to be mostly lowercase.

Format:

- `type(scope optional): short description`

Examples:

- `fix: handle empty response from api`
- `feat(auth): add passkey login`
- `docs: clarify canary workflow`
- `refactor: simplify retry logic`
- `chore(ci): speed up checks`

Breaking changes:

- `feat!: remove legacy auth endpoints`
- `refactor(api)!: change pagination shape`

Common types:
`feat`, `fix`, `docs`, `refactor`, `perf`, `test`, `chore`, `ci`, `build`, `revert`

## Tests (guidance)

We care about confidence more than ceremony. Add tests when they provide real value.

### Backend changes

For backend changes, add a unit test.

- If a unit test would require heavy mocking to be meaningful, restructure the code so it can be tested cleanly through its interfaces.
- If you're unsure how to approach this, discuss it with a maintainer before investing time.

### Frontend changes

We don't generally encourage new unit tests for frontend code unless:

- the area already has unit tests, or
- the change is complex or sensitive, and a unit test clearly reduces risk

In most cases, clear PR notes and practical verification are more valuable.

## Formatting and linting

Don't block on formatting or linting before opening a PR. CI enforces required checks and will tell you what needs fixing before merge.

Open the PR when it's ready for review, then iterate based on CI and feedback.

## PR checklist

Before requesting review:

- [ ] PR targets `canary`
- [ ] PR title follows Conventional Commits (mostly lowercase)
- [ ] PR includes a short description of what/why
- [ ] You understand every change in the PR and can explain it during review
- [ ] Tests added or updated where it makes sense (especially backend changes)
- [ ] CI is green (or you're actively addressing failures)

Optional but helpful:

- screenshots or a short recording for UI changes
- manual verification steps

## Code of Conduct

This project follows a Code of Conduct. By participating, you're expected to uphold it:

- See [`CODE_OF_CONDUCT.md`](./CODE_OF_CONDUCT.md)

## Security

Please don't report security issues via public GitHub issues.

Use our security policy and reporting instructions here:

- https://fluxer.app/security

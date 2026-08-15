# Contributing to Fluxer

This policy applies to all issues, discussions, commits and pull requests.

## Scope

To prevent spam, only approved contributors may submit pull requests.

To request approval, comment on an existing issue and ask to implement it. For work that extends beyond a defect fix, open a [discussion](https://github.com/orgs/fluxerapp/discussions) first.

Every pull request must:

- Target the repository's default branch.
- Include a closing reference for each repository issue it resolves.
- Receive approval from a maintainer before it is merged.

Place each closing reference on a separate line:

```text
Closes #123
Closes #456
```

## Authorship

You must understand every line you submit and be able to explain why the change is correct.

The [LLM usage policy](LLM_USAGE_POLICY.md) defines the authorship requirements for contributors who do not have write access.

Each contribution must contain one coherent change. Do not include unrelated fixes, refactoring or formatting changes.

## Commit requirements

Every commit made by a contributor must include:

- A Developer Certificate of Origin sign-off.
- A cryptographic signature that GitHub marks as verified.

Pull requests opened by Fluxer repository automation are exempt from these commit requirements.

Read the [Developer Certificate of Origin 1.1](https://developercertificate.org) before contributing. Add a `Signed-off-by` trailer by creating the commit with `git commit -s`. The name and email address in the trailer must match those of the commit author or committer.

## Pull request requirements

Pull request titles must contain no more than 72 characters and use the following format:

```text
type(optional-scope): imperative subject
```

The permitted types are:

- `feat`
- `fix`
- `docs`
- `style`
- `refactor`
- `perf`
- `test`
- `build`
- `ci`
- `chore`

For a breaking change, place `!` immediately before the colon:

```text
type(optional-scope)!: imperative subject
```

Prefix the title of a revert with `revert: `.

Complete every section of the pull request template. Clearly describe:

- What the change does.
- Why the change is correct.
- What risks it introduces.
- How it was verified.

## Reports and other contributions

Use the [bug report form](https://github.com/fluxerapp/fluxer/issues/new?template=bug-report.yaml) to report reproducible defects.

Report security vulnerabilities privately through the channels specified in the [security policy](SECURITY.md). Do not report vulnerabilities in public issues or discussions.

Use [discussions](https://github.com/orgs/fluxerapp/discussions) for feature proposals and self-hosting questions.

Submit translations through [Weblate](https://weblate.fluxer.tools), not through pull requests.

All repository activity is governed by the [Code of Conduct](CODE_OF_CONDUCT.md).

Fluxer is distributed under the [GNU Affero General Public License, version 3.0 or later](../LICENSE). By adding a DCO sign-off, you certify that you have the right to submit the contribution under that licence.

## Private marketing project

The marketing implementation is maintained in a private repository at the `fluxer_marketing` submodule path. The public workspace, bootstrap, checks, and development stack work without initializing it.

Authorized maintainers can initialize only that submodule and install its independent dependencies:

```sh
./scripts/setup-private-marketing.sh
pnpm --dir fluxer_marketing install --frozen-lockfile
cargo metadata --locked --manifest-path fluxer_marketing/Cargo.toml
```

To run the private marketing service in the local development stack and direct application links to it, add this override to the ignored `config/env/local.env` file:

```sh
FLUXER_MARKETING_ENDPOINT=http://localhost:8088/marketing
```

# Contributing to Fluxer

This policy applies to all commits and pull requests.

## Scope

To prevent spam, only approved contributors may submit pull requests.

To request approval, comment on the [feedback.fluxer.com](https://feedback.fluxer.com) post you want to implement and ask to work on it. For work that extends beyond a defect fix, post a feature request there first.

Every pull request must:

- Target the repository's default branch.
- Link each feedback.fluxer.com post it resolves.
- Receive approval from a maintainer before it is merged.

Place each link on a separate line:

```text
Resolves https://feedback.fluxer.com/p/123
Resolves https://feedback.fluxer.com/p/456
```

## Authorship

You must understand every line you submit and be able to explain why the change is correct.

The [LLM usage policy](https://github.com/fluxerapp/fluxer/blob/main/.github/LLM_USAGE_POLICY.md) defines the authorship requirements for contributors who do not have write access.

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

## Build version and channel

Two environment variables set the version and channel of a desktop or web build. You can leave both unset for a local build.

| Variable | What it sets | Set by | When unset |
| --- | --- | --- | --- |
| `BUILD_VERSION` | The CalVer version (`YYYY.MDD.MICRO`) of the build. The web app shows it in its build info and `version.json`, and the desktop app uses it as its own version. | The release workflows, or `BUILD_VERSION` as a build argument when you build the app-proxy image yourself. | `dev` for the web app. A desktop package uses the version of the web app it bundles. |
| `BUILD_CHANNEL` | `stable`, `canary` or `development`. For the desktop app it decides the app ID, data folder, link protocol and update feed. For the web app it is the channel used when the page has no channel tag. | The desktop release workflows. | `canary` for the web app and `stable` for the desktop app. |

The app-proxy sets the channel of the web app when it serves the page, using its own `RELEASE_CHANNEL` setting, and the image build leaves `BUILD_CHANNEL` unset.

A build stops with an error when it finds one of the old names `PUBLIC_BUILD_VERSION`, `PUBLIC_RELEASE_CHANNEL`, `RELEASE_CHANNEL` or `VERSION` without the new name.

## Reports and other contributions

Report bugs and request features at [feedback.fluxer.com](https://feedback.fluxer.com).

Report security vulnerabilities privately through [fluxer.app/security](https://fluxer.app/security). Never post them publicly.

Read the [operator documentation](https://fluxer.dev) for self-hosting questions.

Submit translations through [Weblate](https://weblate.fluxer.tools), not through pull requests.

All repository activity is governed by the [Code of Conduct](https://github.com/fluxerapp/fluxer/blob/main/.github/CODE_OF_CONDUCT.md).

Fluxer is distributed under the [GNU Affero General Public License, version 3.0 or later](https://github.com/fluxerapp/fluxer/blob/main/LICENSE). By adding a DCO sign-off, you certify that you have the right to submit the contribution under that licence.

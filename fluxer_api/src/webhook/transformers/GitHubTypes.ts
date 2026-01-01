/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createStringType, Int32Type, URLType, z} from '~/Schema';

const GitHubUser = z.object({
	id: Int32Type,
	login: createStringType(0, 152133),
	html_url: URLType,
	avatar_url: URLType,
});

const GitHubCheckPullRequest = z.object({
	number: Int32Type,
});

const GitHubCheckApp = z.object({
	name: createStringType(0, 152133),
});

const GitHubCheckSuite = z.object({
	conclusion: createStringType(0, 152133).nullish(),
	head_branch: createStringType(0, 152133).nullish(),
	head_sha: createStringType(0, 152133),
	pull_requests: z.array(GitHubCheckPullRequest).nullish(),
	app: GitHubCheckApp,
});

const GitHubCheckRunOutput = z.object({
	title: createStringType(0, 152133).nullish(),
	summary: createStringType(0, 152133).nullish(),
});

const GitHubAuthor = z.object({
	username: createStringType(0, 152133).nullish(),
	name: createStringType(0, 152133),
});

const GitHubCheckRun = z.object({
	conclusion: createStringType(0, 152133).nullish(),
	name: createStringType(0, 152133),
	html_url: URLType,
	check_suite: GitHubCheckSuite,
	details_url: URLType.nullish(),
	output: GitHubCheckRunOutput.nullish(),
	pull_requests: z.array(GitHubCheckPullRequest).nullish(),
});

const GitHubComment = z.object({
	id: Int32Type,
	html_url: URLType,
	user: GitHubUser,
	commit_id: createStringType(0, 152133).nullish(),
	body: createStringType(0, 152133),
});

const GitHubCommit = z.object({
	id: createStringType(0, 152133),
	url: URLType,
	message: createStringType(0, 152133),
	author: GitHubAuthor,
});

const GitHubDiscussion = z.object({
	title: createStringType(0, 152133),
	number: Int32Type,
	html_url: URLType,
	answer_html_url: URLType.nullish(),
	body: createStringType(0, 152133).nullish(),
	user: GitHubUser,
});

const GitHubIssue = z.object({
	id: Int32Type,
	number: Int32Type,
	html_url: URLType,
	user: GitHubUser,
	title: createStringType(0, 152133),
	body: createStringType(0, 152133).nullish(),
});

const GitHubRelease = z.object({
	id: Int32Type,
	tag_name: createStringType(0, 152133),
	html_url: URLType,
	body: createStringType(0, 152133).nullish(),
});

const GitHubService = z.object({
	id: Int32Type,
	html_url: URLType,
	name: createStringType(0, 152133),
	full_name: createStringType(0, 152133),
});

const GitHubReview = z.object({
	user: GitHubUser,
	body: createStringType(0, 152133).nullish(),
	html_url: URLType,
	state: createStringType(0, 152133),
});

export const GitHubWebhook = z.object({
	action: createStringType(0, 152133).nullish(),
	answer: GitHubComment.nullish(),
	check_run: GitHubCheckRun.nullish(),
	check_suite: GitHubCheckSuite.nullish(),
	comment: GitHubComment.nullish(),
	commits: z.array(GitHubCommit).nullish(),
	compare: URLType.nullish(),
	discussion: GitHubDiscussion.nullish(),
	forced: z.boolean().nullish(),
	forkee: GitHubService.nullish(),
	head_commit: GitHubCommit.nullish(),
	issue: GitHubIssue.nullish(),
	member: GitHubUser.nullish(),
	pull_request: GitHubIssue.nullish(),
	ref_type: createStringType(0, 152133).nullish(),
	ref: createStringType(0, 152133).nullish(),
	release: GitHubRelease.nullish(),
	repository: GitHubService.nullish(),
	review: GitHubReview.nullish(),
	sender: GitHubUser,
});

export type GitHubWebhook = z.infer<typeof GitHubWebhook>;

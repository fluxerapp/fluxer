// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseString} from '@app/api/utils/StringUtils';
import type {RichEmbedRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {GitHubWebhook} from '@fluxer/schema/src/domains/webhook/GitHubWebhookSchemas';

export async function transformCheckRun(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
	if (!body.repository || body.action !== 'completed' || !body.check_run) {
		return null;
	}
	const checkRun = body.check_run;
	if (checkRun.check_suite.conclusion === 'skipped') {
		return null;
	}
	const commitUrl = `${body.repository.html_url}/commit/${checkRun.check_suite.head_sha}`;
	return {
		url: commitUrl,
		title: parseString(
			`[${body.repository.name}] ${checkRun.name} ${checkRun.conclusion} on ${checkRun.check_suite.head_branch}`,
			256,
		),
		color: checkRun.conclusion === 'success' ? 0x009800 : 0xfc2929,
	};
}

export async function transformCheckSuite(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
	if (!body.repository || body.action !== 'completed' || !body.check_suite) {
		return null;
	}
	const checkSuite = body.check_suite;
	if (checkSuite.conclusion === 'skipped') {
		return null;
	}
	const commitUrl = `${body.repository.html_url}/commit/${checkSuite.head_sha}`;
	return {
		url: commitUrl,
		title: parseString(
			`[${body.repository.name}] ${checkSuite.app.name} checks ${checkSuite.conclusion} on ${checkSuite.head_branch}`,
			256,
		),
		color: checkSuite.conclusion === 'success' ? 0x009800 : 0xfc2929,
	};
}

export async function transformDiscussion(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
	if (body.action !== 'created' || !body.discussion || !body.repository) {
		return null;
	}
	const authorIconUrl = body.discussion.user.avatar_url;
	const authorName = body.discussion.user.login;
	const authorUrl = body.discussion.user.html_url;
	const repoName = body.repository.full_name;
	const discussionNumber = body.discussion.number;
	const discussionTitle = body.discussion.title;
	const discussionUrl = body.discussion.html_url;
	const discussionBody = body.discussion.body;
	const color = 0xe6c2b0;
	const title = `[${repoName}] New discussion #${discussionNumber}: ${discussionTitle}`;
	return {
		title: parseString(title, 70),
		url: discussionUrl,
		color,
		description: parseString(discussionBody ?? '', 350),
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
}

export async function transformDiscussionComment(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
	if (body.action !== 'created' || !body.comment || !body.discussion || !body.repository) {
		return null;
	}
	const authorIconUrl = body.comment.user.avatar_url;
	const authorName = body.comment.user.login;
	const authorUrl = body.comment.user.html_url;
	const repoName = body.repository.full_name;
	const discussionNumber = body.discussion.number;
	const discussionTitle = body.discussion.title;
	const commentUrl = body.comment.html_url;
	const commentBody = body.comment.body;
	const color = 0xe6c2b0;
	const title = `[${repoName}] New comment on discussion #${discussionNumber}: ${discussionTitle}`;
	return {
		title: parseString(title, 70),
		url: commentUrl,
		color,
		description: parseString(commentBody, 350),
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
}

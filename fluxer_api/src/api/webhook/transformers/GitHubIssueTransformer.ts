// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseString} from '@app/api/utils/StringUtils';
import type {RichEmbedRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {GitHubWebhook} from '@fluxer/schema/src/domains/webhook/GitHubWebhookSchemas';

export async function transformIssue(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
	if (!(body.issue && body.action && body.repository)) {
		return null;
	}
	const authorIconUrl = body.sender.avatar_url;
	const authorName = body.sender.login;
	const authorUrl = body.sender.html_url;
	const repoName = body.repository.full_name;
	const issueNumber = body.issue.number;
	const issueTitle = body.issue.title;
	const issueUrl = body.issue.html_url;
	const issueDescription = body.issue.body || '';
	let title: string;
	let color: number;
	switch (body.action) {
		case 'opened': {
			title = `[${repoName}] Issue opened: #${issueNumber} ${issueTitle}`;
			color = 0xeb4841;
			break;
		}
		case 'closed': {
			title = `[${repoName}] Issue closed: #${issueNumber} ${issueTitle}`;
			color = 0x000000;
			break;
		}
		case 'reopened': {
			title = `[${repoName}] Issue reopened: #${issueNumber} ${issueTitle}`;
			color = 0xfcbd1f;
			break;
		}
		default:
			return null;
	}
	return {
		title: parseString(title, 70),
		url: issueUrl,
		color,
		description: body.action === 'opened' ? parseString(issueDescription, 350) : undefined,
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
}

export async function transformIssueComment(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
	if (!body.comment || body.action !== 'created' || !body.issue || !body.repository) {
		return null;
	}
	const authorIconUrl = body.comment.user.avatar_url;
	const authorName = body.comment.user.login;
	const authorUrl = body.comment.user.html_url;
	const repoName = body.repository.full_name;
	const issueNumber = body.issue.number;
	const issueTitle = body.issue.title;
	const commentUrl = body.comment.html_url;
	const commentBody = body.comment.body;
	const isPullRequest = body.pull_request != null;
	const titlePrefix = isPullRequest ? 'pull request' : 'issue';
	const title = `[${repoName}] New comment on ${titlePrefix} #${issueNumber}: ${issueTitle}`;
	const color = 0xc00a7f;
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

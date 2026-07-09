// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RichEmbedRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {GitHubWebhook} from '@fluxer/schema/src/domains/webhook/GitHubWebhookSchemas';
import {parseString} from '../../utils/StringUtils';

function parseDescription(value: string | null | undefined): string | undefined {
	if (!value) return undefined;
	const description = parseString(value, 350);
	return description.length > 0 ? description : undefined;
}

export async function transformPullRequest(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
	if (!(body.pull_request && body.action && body.repository)) {
		return null;
	}
	const authorIconUrl = body.sender.avatar_url;
	const authorName = body.sender.login;
	const authorUrl = body.sender.html_url;
	const repoName = body.repository.full_name;
	const prNumber = body.pull_request.number;
	const prTitle = body.pull_request.title;
	const prUrl = body.pull_request.html_url;
	const prDescription = parseDescription(body.pull_request.body);
	let title: string;
	let color: number;
	switch (body.action) {
		case 'opened': {
			title = `[${repoName}] Pull request opened: #${prNumber} ${prTitle}`;
			color = 0x098efc;
			break;
		}
		case 'closed': {
			title = `[${repoName}] Pull request closed: #${prNumber} ${prTitle}`;
			color = 0x000000;
			break;
		}
		case 'reopened': {
			title = `[${repoName}] Pull request reopened: #${prNumber} ${prTitle}`;
			color = 0xfcbd1f;
			break;
		}
		default:
			return null;
	}
	return {
		title: parseString(title, 70),
		url: prUrl,
		color,
		description: body.action === 'opened' ? prDescription : undefined,
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
}

export async function transformPullRequestReview(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
	if (!body.review || body.action !== 'submitted' || !body.pull_request || !body.repository) {
		return null;
	}
	const authorIconUrl = body.review.user.avatar_url;
	const authorName = body.review.user.login;
	const authorUrl = body.review.user.html_url;
	const repoName = body.repository.full_name;
	const prNumber = body.pull_request.number;
	const prTitle = body.pull_request.title;
	const reviewUrl = body.review.html_url;
	const reviewBody = parseDescription(body.review.body);
	const title = `[${repoName}] Pull request review submitted: #${prNumber} ${prTitle}`;
	const color = 0x000000;
	return {
		title: parseString(title, 70),
		url: reviewUrl,
		color,
		description: reviewBody,
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
}

export async function transformPullRequestReviewComment(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
	if (!body.comment || body.action !== 'created' || !body.pull_request || !body.repository) {
		return null;
	}
	const authorIconUrl = body.comment.user.avatar_url;
	const authorName = body.comment.user.login;
	const authorUrl = body.comment.user.html_url;
	const repoName = body.repository.full_name;
	const prNumber = body.pull_request.number;
	const prTitle = body.pull_request.title;
	const commentUrl = body.comment.html_url;
	const commentBody = parseDescription(body.comment.body);
	const title = `[${repoName}] New review comment on pull request #${prNumber}: ${prTitle}`;
	const color = 0xc00a7f;
	return {
		title: parseString(title, 70),
		url: commentUrl,
		color,
		description: commentBody,
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
}

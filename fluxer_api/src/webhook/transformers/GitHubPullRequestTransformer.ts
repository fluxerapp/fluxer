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

import type {RichEmbedRequest} from '~/channel/ChannelModel';
import {parseString} from '~/utils/StringUtils';
import type {GitHubWebhook} from './GitHubTypes';

export const transformPullRequest = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
	if (!(body.pull_request && body.action && body.repository)) {
		return null;
	}

	const authorIconUrl = body.pull_request.user.avatar_url;
	const authorName = body.pull_request.user.login;
	const authorUrl = body.pull_request.user.html_url;
	const repoName = body.repository.full_name;
	const prNumber = body.pull_request.number;
	const prTitle = body.pull_request.title;
	const prUrl = body.pull_request.html_url;
	const prDescription = body.pull_request.body || '';

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
		description: body.action === 'opened' ? parseString(prDescription, 350) : undefined,
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
};

export const transformPullRequestReview = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
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
	const reviewBody = body.review.body || 'No description provided';

	const title = `[${repoName}] Pull request review submitted: #${prNumber} ${prTitle}`;
	const color = 0x000000;

	return {
		title: parseString(title, 70),
		url: reviewUrl,
		color,
		description: parseString(reviewBody, 350),
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
};

export const transformPullRequestReviewComment = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
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
	const commentBody = body.comment.body || 'No description provided';

	const title = `[${repoName}] New review comment on pull request #${prNumber}: ${prTitle}`;
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
};

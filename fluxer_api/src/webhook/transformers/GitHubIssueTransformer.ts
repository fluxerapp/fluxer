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

export const transformIssue = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
	if (!(body.issue && body.action && body.repository)) {
		return null;
	}

	const authorIconUrl = body.issue.user.avatar_url;
	const authorName = body.issue.user.login;
	const authorUrl = body.issue.user.html_url;
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
};

export const transformIssueComment = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
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
};

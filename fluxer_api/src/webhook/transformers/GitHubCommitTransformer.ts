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

export const transformCommitComment = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
	if (!body.repository || body.action !== 'created' || !body.comment) {
		return null;
	}
	const comment = body.comment;
	if (!comment.commit_id) {
		return null;
	}
	const commitId = comment.commit_id.substring(0, 7);
	return {
		url: comment.html_url,
		title: parseString(`[${body.repository.full_name}] New comment on commit \`${commitId}\``, 70),
		description: parseString(comment.body, 350),
		author: {
			name: comment.user.login,
			url: comment.user.html_url,
			icon_url: comment.user.avatar_url,
		},
	};
};

export const transformCreate = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
	if (!(body.ref && body.ref_type && body.repository)) {
		return null;
	}

	const authorIconUrl = body.sender.avatar_url;
	const authorName = body.sender.login;
	const authorUrl = body.sender.html_url;
	const repoName = body.repository.full_name;
	const ref = body.ref;
	const refType = body.ref_type;

	let title: string;
	if (refType === 'branch') {
		title = `[${repoName}] New branch created: ${ref}`;
	} else if (refType === 'tag') {
		title = `[${repoName}] New tag created: ${ref}`;
	} else {
		return null;
	}

	return {
		title: parseString(title, 70),
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
};

export const transformDelete = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
	if (!(body.ref && body.ref_type && body.repository)) {
		return null;
	}

	const authorIconUrl = body.sender.avatar_url;
	const authorName = body.sender.login;
	const authorUrl = body.sender.html_url;
	const repoName = body.repository.full_name;
	const ref = body.ref;
	const refType = body.ref_type;

	let title: string;
	if (refType === 'branch') {
		title = `[${repoName}] Branch deleted: ${ref}`;
	} else if (refType === 'tag') {
		title = `[${repoName}] Tag deleted: ${ref}`;
	} else {
		return null;
	}

	return {
		title: parseString(title, 70),
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
};

export const transformPush = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
	if (!(body.repository && body.ref)) {
		return null;
	}

	const authorIconUrl = body.sender.avatar_url;
	const authorName = body.sender.login;
	const authorUrl = body.sender.html_url;
	const repoName = body.repository.name;
	const ref = body.ref;

	let refName: string;
	let refType: string;
	if (ref.startsWith('refs/heads/')) {
		refName = ref.replace('refs/heads/', '');
		refType = 'Branch';
	} else if (ref.startsWith('refs/tags/')) {
		refName = ref.replace('refs/tags/', '');
		refType = 'Tag';
	} else {
		refName = ref;
		refType = 'Ref';
	}

	if (body.forced) {
		if (!(body.head_commit && body.compare)) {
			return null;
		}

		const shortAfterCommitId = body.head_commit.id.substring(0, 7);

		return {
			url: body.compare,
			title: parseString(`[${repoName}] ${refType} ${refName} was force-pushed to \`${shortAfterCommitId}\``, 70),
			color: 0xfcbd1f,
			description: `[Compare changes](${body.compare})`,
			author: {
				name: authorName,
				url: authorUrl,
				icon_url: authorIconUrl,
			},
		};
	}

	if (!body.commits || body.commits.length === 0) {
		return null;
	}

	const commitDescriptions = body.commits
		.map((commit) => {
			const shortCommitId = commit.id.substring(0, 7);
			const commitMessage = commit.message.replace(
				/This reverts commit (\w{40})\./g,
				(_, hash) => `This reverts commit [\`${hash.substring(0, 7)}\`](${body.repository?.html_url}/commit/${hash}).`,
			);
			return `[\`${shortCommitId}\`](${commit.url}) ${commitMessage} - ${commit.author.name}`;
		})
		.join('\n');

	if (!body.compare) {
		return null;
	}

	return {
		url: body.compare,
		title: parseString(
			`[${repoName}:${refName}] ${body.commits.length} new commit${body.commits.length > 1 ? 's' : ''}`,
			256,
		),
		color: 0x7289da,
		description: parseString(commitDescriptions, 350),
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
};

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

export const transformFork = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
	if (!(body.repository && body.forkee && body.sender)) {
		return null;
	}
	const forkee = body.forkee;
	const sender = body.sender;
	return {
		url: forkee.html_url,
		title: parseString(`[${body.repository.full_name}] Fork created: ${forkee.full_name}`, 70),
		author: {
			name: sender.login,
			url: sender.html_url,
			icon_url: sender.avatar_url,
		},
	};
};

export const transformMember = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
	if (body.action !== 'added' || !body.member || !body.repository) {
		return null;
	}

	const authorIconUrl = body.sender.avatar_url;
	const authorName = body.sender.login;
	const authorUrl = body.sender.html_url;
	const repoName = body.repository.full_name;
	const memberName = body.member.login;
	const memberUrl = body.member.html_url;

	const title = `[${repoName}] New collaborator added: ${memberName}`;

	return {
		title: parseString(title, 70),
		url: memberUrl,
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
};

export const transformPublic = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
	if (!body.repository) {
		return null;
	}

	const authorIconUrl = body.sender.avatar_url;
	const authorName = body.sender.login;
	const authorUrl = body.sender.html_url;
	const repoName = body.repository.full_name;

	const title = `[${repoName}] Now open sourced!`;

	return {
		title: parseString(title, 70),
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
};

export const transformRelease = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
	if (!(body.action === 'published' && body.release && body.repository)) {
		return null;
	}

	const authorIconUrl = body.sender.avatar_url;
	const authorName = body.sender.login;
	const authorUrl = body.sender.html_url;
	const repoName = body.repository.full_name;
	const releaseTag = body.release.tag_name;
	const releaseUrl = body.release.html_url;

	return {
		title: parseString(`[${repoName}] New release published: ${releaseTag}`, 70),
		url: releaseUrl,
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
};

export const transformWatch = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
	if (body.action !== 'started' || !body.repository) {
		return null;
	}

	const authorIconUrl = body.sender.avatar_url;
	const authorName = body.sender.login;
	const authorUrl = body.sender.html_url;
	const repoName = body.repository.full_name;
	const repoUrl = body.repository.html_url;

	const title = `[${repoName}] New star added`;

	return {
		title: parseString(title, 70),
		url: repoUrl,
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
};

export const transformRepository = async (body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
	if (body.action !== 'created' || !body.repository) {
		return null;
	}

	const authorIconUrl = body.sender.avatar_url;
	const authorName = body.sender.login;
	const authorUrl = body.sender.html_url;
	const repoName = body.repository.full_name;
	const repoUrl = body.repository.html_url;

	const title = `[${repoName}] Repository created`;

	return {
		title: parseString(title, 70),
		url: repoUrl,
		color: 0x2cbe4e,
		author: {
			name: authorName,
			url: authorUrl,
			icon_url: authorIconUrl,
		},
	};
};

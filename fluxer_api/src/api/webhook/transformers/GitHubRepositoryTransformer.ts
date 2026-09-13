// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseString} from '@app/api/utils/StringUtils';
import type {RichEmbedRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {GitHubWebhook} from '@fluxer/schema/src/domains/webhook/GitHubWebhookSchemas';

export async function transformFork(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
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
}

export async function transformMember(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
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
}

export async function transformPublic(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
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
}

export async function transformRelease(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
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
}

export async function transformWatch(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
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
}

export async function transformRepository(body: GitHubWebhook): Promise<RichEmbedRequest | null> {
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
}

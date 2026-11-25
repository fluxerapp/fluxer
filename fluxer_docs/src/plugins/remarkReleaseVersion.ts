// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RemarkPlugin} from '@astrojs/markdown-remark';

const RELEASE_TOKEN = '{{FLUXER_VERSION}}';
const FALLBACK_RELEASE = '2026.1123.15517';

const LATEST_URL = 'https://github.com/fluxerapp/fluxer/releases/latest';
const TAG_PREFIX = 'fluxer-release@';
const CALVER = /^\d{4}\.\d{3,4}\.\d{1,6}$/;
const FETCH_TIMEOUT_MS = 10 * 1000;

interface MarkdownNode {
	type: string;
	value?: string;
	children?: Array<MarkdownNode>;
}

export async function newestRelease(
	env: NodeJS.ProcessEnv = process.env,
	fetchLatest: () => Promise<string | null> = fetchLatestTag,
): Promise<string> {
	const pinned = env.FLUXER_DOCS_RELEASE?.trim();
	if (pinned) {
		if (!CALVER.test(pinned)) {
			throw new Error(`FLUXER_DOCS_RELEASE is ${pinned}, which isn't a Fluxer release version`);
		}
		return pinned;
	}
	const tag = await fetchLatest().catch(() => null);
	const version = tag?.startsWith(TAG_PREFIX) ? tag.slice(TAG_PREFIX.length) : '';
	if (CALVER.test(version)) {
		return version;
	}
	console.warn(`Couldn't read the newest release from ${LATEST_URL}, so code shows ${FALLBACK_RELEASE}`);
	return FALLBACK_RELEASE;
}

function tagFromLocation(location: string | null): string | null {
	const tag = location?.match(/\/releases\/tag\/([^/?#]+)$/u)?.[1];
	return tag ? decodeURIComponent(tag) : null;
}

async function fetchLatestTag(fetcher: typeof fetch = fetch): Promise<string | null> {
	const response = await fetcher(LATEST_URL, {
		method: 'HEAD',
		redirect: 'manual',
		signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
	});
	return tagFromLocation(response.headers.get('location'));
}

function fillReleaseVersion(node: MarkdownNode, version: string): void {
	if ((node.type === 'code' || node.type === 'inlineCode') && node.value?.includes(RELEASE_TOKEN)) {
		node.value = node.value.replaceAll(RELEASE_TOKEN, version);
	}
	for (const child of node.children ?? []) {
		fillReleaseVersion(child, version);
	}
}

export const remarkReleaseVersion = (release: () => Promise<string>): RemarkPlugin => {
	let version: Promise<string> | null = null;
	return () => async (tree) => {
		version ??= release();
		fillReleaseVersion(tree as MarkdownNode, await version);
	};
};

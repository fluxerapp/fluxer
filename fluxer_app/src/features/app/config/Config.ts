// SPDX-License-Identifier: AGPL-3.0-or-later

import * as v from 'valibot';

const buildInfoSchema = v.object({
	BUILD_VERSION: v.nullish(v.string(), 'dev'),
	BUILD_CHANNEL: v.picklist(['stable', 'canary', 'development']),
});
class BuildInfoInvariantError extends Error {
	constructor(issues: string) {
		super(
			`Invalid build info: ${issues}. rspack injects BUILD_VERSION and BUILD_CHANNEL at build time. BUILD_CHANNEL must be stable, canary or development.`,
		);
		this.name = 'BuildInfoInvariantError';
	}
}

const parsedBuildInfo = v.safeParse(buildInfoSchema, {
	BUILD_VERSION: import.meta.env.BUILD_VERSION,
	BUILD_CHANNEL: import.meta.env.BUILD_CHANNEL,
});
if (!parsedBuildInfo.success) {
	throw new BuildInfoInvariantError(parsedBuildInfo.issues.map((issue) => issue.message).join(', '));
}
const buildInfo = parsedBuildInfo.output;

const SERVED_RELEASE_CHANNEL_META_SELECTOR = 'meta[name="fluxer-release-channel"]';

function servedReleaseChannel(): 'stable' | 'canary' | null {
	if (typeof document === 'undefined') {
		return null;
	}
	const value = document.querySelector<HTMLMetaElement>(SERVED_RELEASE_CHANNEL_META_SELECTOR)?.content;
	return value === 'stable' || value === 'canary' ? value : null;
}

function clientReleaseChannel(value: (typeof buildInfo)['BUILD_CHANNEL']): 'stable' | 'canary' {
	switch (value) {
		case 'stable':
			return 'stable';
		case 'canary':
		case 'development':
			return 'canary';
	}
}

export default {
	BUILD_VERSION: buildInfo.BUILD_VERSION,
	RELEASE_CHANNEL: servedReleaseChannel() ?? clientReleaseChannel(buildInfo.BUILD_CHANNEL),
} as const;

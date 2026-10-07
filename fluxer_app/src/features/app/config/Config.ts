// SPDX-License-Identifier: AGPL-3.0-or-later

import * as v from 'valibot';

const buildInfoSchema = v.object({
	PUBLIC_BUILD_VERSION: v.nullish(v.string(), 'dev'),
	PUBLIC_RELEASE_CHANNEL: v.picklist(['stable', 'canary', 'development']),
});
class BuildInfoInvariantError extends Error {
	constructor(issues: string) {
		super(
			`Invalid build info: ${issues}. PUBLIC_BUILD_VERSION and PUBLIC_RELEASE_CHANNEL are injected by rspack at build time and by fluxer_app/vitest.config.ts test.env under test. PUBLIC_RELEASE_CHANNEL must be stable, canary or development.`,
		);
		this.name = 'BuildInfoInvariantError';
	}
}

const parsedBuildInfo = v.safeParse(buildInfoSchema, {
	PUBLIC_BUILD_VERSION: import.meta.env.PUBLIC_BUILD_VERSION,
	PUBLIC_RELEASE_CHANNEL: import.meta.env.PUBLIC_RELEASE_CHANNEL,
});
if (!parsedBuildInfo.success) {
	throw new BuildInfoInvariantError(parsedBuildInfo.issues.map((issue) => issue.message).join(', '));
}
const buildInfo = parsedBuildInfo.output;

function clientReleaseChannel(value: (typeof buildInfo)['PUBLIC_RELEASE_CHANNEL']): 'stable' | 'canary' {
	switch (value) {
		case 'stable':
			return 'stable';
		case 'canary':
		case 'development':
			return 'canary';
	}
}

export default {
	PUBLIC_BUILD_VERSION: buildInfo.PUBLIC_BUILD_VERSION,
	PUBLIC_RELEASE_CHANNEL: clientReleaseChannel(buildInfo.PUBLIC_RELEASE_CHANNEL),
} as const;

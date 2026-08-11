// SPDX-License-Identifier: AGPL-3.0-or-later

import {createNamedStringLiteralUnion, withOpenApiType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const DesktopChannelEnum = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['stable', 'Stable', 'The stable release channel for production use'],
			['canary', 'Canary', 'The canary release channel for early access to new features'],
		],
		'The release channel',
	),
	'DesktopChannel',
);

export type DesktopChannel = z.infer<typeof DesktopChannelEnum>;

export const DesktopPlatformEnum = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['win32', 'Windows', 'Microsoft Windows operating system'],
			['darwin', 'macOS', 'Apple macOS operating system'],
			['linux', 'Linux', 'Linux operating system'],
		],
		'The operating system platform',
	),
	'DesktopPlatform',
);

export type DesktopPlatform = z.infer<typeof DesktopPlatformEnum>;

export const DesktopArchEnum = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['x64', 'x64', '64-bit x86 architecture (Intel/AMD)'],
			['arm64', 'ARM64', '64-bit ARM architecture (Apple Silicon, ARM processors)'],
		],
		'The CPU architecture',
	),
	'DesktopArch',
);

export type DesktopArch = z.infer<typeof DesktopArchEnum>;

export const DesktopFormatEnum = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['setup', 'Setup', 'Windows installer executable'],
			['dmg', 'DMG', 'macOS disk image'],
			['zip', 'ZIP', 'Compressed archive'],
			['appimage', 'AppImage', 'Linux portable application'],
			['deb', 'DEB', 'Debian/Ubuntu package'],
			['rpm', 'RPM', 'Red Hat/Fedora package'],
			['tar_gz', 'TAR.GZ', 'Compressed tarball archive'],
			['portable', 'Portable', 'Windows portable ZIP archive (no installer, stores data next to the executable)'],
		],
		'The package format',
	),
	'DesktopFormat',
);

export type DesktopFormat = z.infer<typeof DesktopFormatEnum>;

const VersionString = z
	.string()
	.regex(/^\d+\.\d+\.\d+$/u)
	.describe('Semantic version string');
const TestBuildFlag = z
	.string()
	.optional()
	.transform((value) => value === '1' || value?.toLowerCase() === 'true')
	.describe('When set to 1/true, resolve against the desktop-test/ bucket prefix instead of desktop/.');
export const DesktopTestBuildQuery = z.object({
	test: TestBuildFlag,
});

export type DesktopTestBuildQuery = z.infer<typeof DesktopTestBuildQuery>;

export const DesktopRedirectParam = z.object({
	channel: DesktopChannelEnum,
	plat: DesktopPlatformEnum,
	arch: DesktopArchEnum,
	format: DesktopFormatEnum,
});

export type DesktopRedirectParam = z.infer<typeof DesktopRedirectParam>;

export const DesktopVersionedRedirectParam = z.object({
	channel: DesktopChannelEnum,
	plat: DesktopPlatformEnum,
	arch: DesktopArchEnum,
	version: VersionString,
	format: DesktopFormatEnum,
});

export type DesktopVersionedRedirectParam = z.infer<typeof DesktopVersionedRedirectParam>;

const DesktopChecksumFormat = z
	.string()
	.regex(/^(setup|dmg|zip|appimage|deb|rpm|tar_gz|portable)\.sha256$/u)
	.transform((value) => value.slice(0, -'.sha256'.length) as DesktopFormat)
	.describe('Package format followed by .sha256');

export const DesktopChecksumRedirectParam = z.object({
	channel: DesktopChannelEnum,
	plat: DesktopPlatformEnum,
	arch: DesktopArchEnum,
	format: DesktopChecksumFormat,
});

export type DesktopChecksumRedirectParam = z.infer<typeof DesktopChecksumRedirectParam>;

export const DesktopVersionedChecksumRedirectParam = z.object({
	channel: DesktopChannelEnum,
	plat: DesktopPlatformEnum,
	arch: DesktopArchEnum,
	version: VersionString,
	format: DesktopChecksumFormat,
});

export type DesktopVersionedChecksumRedirectParam = z.infer<typeof DesktopVersionedChecksumRedirectParam>;

export const DesktopVersionsParam = z.object({
	channel: DesktopChannelEnum,
	plat: DesktopPlatformEnum,
	arch: DesktopArchEnum,
});

export type DesktopVersionsParam = z.infer<typeof DesktopVersionsParam>;

export const DesktopVersionsQuery = z.object({
	limit: z.coerce.number().int().min(1).max(100).default(25).describe('Maximum number of versions to return'),
	before: VersionString.optional().describe('Return versions before this version'),
	after: VersionString.optional().describe('Return versions after this version'),
	test: TestBuildFlag,
});

export type DesktopVersionsQuery = z.infer<typeof DesktopVersionsQuery>;

const VersionFileResponse = z.object({
	url: z.string().describe('Download URL for this file'),
	sha256: z.string().nullable().describe('SHA-256 hash of the file for verification'),
	checksum_url: z.string().nullable().describe('Plain text .sha256 checksum file URL for this file'),
});

export const VersionInfoResponse = z.object({
	version: z.string().describe('Semantic version string (e.g., 1.0.0)'),
	pub_date: z.string().describe('ISO 8601 date when this version was published'),
	minimum_system_version: z
		.string()
		.nullable()
		.optional()
		.describe('Minimum operating system version required by this release, when applicable'),
	files: z
		.record(DesktopFormatEnum, VersionFileResponse.optional())
		.describe('Map of package format to download files'),
});

export type VersionInfoResponse = z.infer<typeof VersionInfoResponse>;

export const DesktopVersionsResponse = z.object({
	versions: z.array(VersionInfoResponse).describe('Array of available versions'),
	has_more: z.boolean().describe('Whether more versions are available to fetch'),
});

export type DesktopVersionsResponse = z.infer<typeof DesktopVersionsResponse>;

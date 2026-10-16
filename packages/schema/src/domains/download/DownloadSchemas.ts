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

const VersionString = z
	.string()
	.regex(/^\d+\.\d+\.\d+$/u)
	.describe('Semantic version string');

export const DesktopVersionsParam = z.object({
	channel: DesktopChannelEnum,
	plat: DesktopPlatformEnum,
	arch: DesktopArchEnum,
});

export type DesktopVersionsParam = z.infer<typeof DesktopVersionsParam>;

export const DesktopRedirectParam = DesktopVersionsParam.extend({
	format: DesktopFormatEnum,
});

export type DesktopRedirectParam = z.infer<typeof DesktopRedirectParam>;

export const DesktopVersionedRedirectParam = DesktopRedirectParam.extend({
	version: VersionString,
});

export type DesktopVersionedRedirectParam = z.infer<typeof DesktopVersionedRedirectParam>;

const DesktopChecksumFormat = z
	.templateLiteral([DesktopFormatEnum, '.sha256'])
	.transform((value) => value.slice(0, -'.sha256'.length))
	.pipe(DesktopFormatEnum)
	.describe('Package format followed by .sha256');

export const DesktopChecksumRedirectParam = DesktopVersionsParam.extend({
	format: DesktopChecksumFormat,
});

export type DesktopChecksumRedirectParam = z.infer<typeof DesktopChecksumRedirectParam>;

export const DesktopVersionedChecksumRedirectParam = DesktopChecksumRedirectParam.extend({
	version: VersionString,
});

export type DesktopVersionedChecksumRedirectParam = z.infer<typeof DesktopVersionedChecksumRedirectParam>;

const DesktopZsyncFormat = z
	.templateLiteral([DesktopFormatEnum, '.zsync'])
	.transform((value) => value.slice(0, -'.zsync'.length))
	.pipe(z.literal('appimage'))
	.describe('Package format followed by .zsync, which only appimage publishes');

export const DesktopZsyncRedirectParam = DesktopVersionsParam.extend({
	format: DesktopZsyncFormat,
});

export type DesktopZsyncRedirectParam = z.infer<typeof DesktopZsyncRedirectParam>;

export const DesktopVersionedZsyncRedirectParam = DesktopZsyncRedirectParam.extend({
	version: VersionString,
});

export type DesktopVersionedZsyncRedirectParam = z.infer<typeof DesktopVersionedZsyncRedirectParam>;

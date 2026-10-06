// SPDX-License-Identifier: AGPL-3.0-or-later

import {CANARY_RELEASE_CHANNEL_NAME, PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import type {UpdaterDownloadFormat, UpdaterDownloadOption} from '@app/features/platform/types/Electron';

type LinuxManualDownloadFormat = Extract<UpdaterDownloadFormat, 'appimage' | 'deb' | 'rpm' | 'tar_gz'>;
type LinuxDownloadArch = 'x64' | 'arm64';
type DesktopDownloadChannel = 'stable' | 'canary';

const LINUX_MANUAL_DOWNLOAD_FORMATS: ReadonlyArray<LinuxManualDownloadFormat> = ['appimage', 'deb', 'rpm', 'tar_gz'];

const LINUX_MANUAL_FORMAT_LABELS: Record<LinuxManualDownloadFormat, string> = {
	appimage: 'AppImage',
	deb: 'DEB package',
	rpm: 'RPM package',
	tar_gz: 'tar.gz archive',
};

const LINUX_MANUAL_FORMAT_EXTENSIONS: Record<LinuxManualDownloadFormat, string> = {
	appimage: '.AppImage',
	deb: '.deb',
	rpm: '.rpm',
	tar_gz: '.tar.gz',
};

const LINUX_MANUAL_ARCH_TOKENS: Record<LinuxManualDownloadFormat, Record<LinuxDownloadArch, string>> = {
	appimage: {x64: 'x86_64', arm64: 'arm64'},
	deb: {x64: 'amd64', arm64: 'arm64'},
	rpm: {x64: 'x86_64', arm64: 'aarch64'},
	tar_gz: {x64: 'x64', arm64: 'arm64'},
};

const PACKAGE_ORIGIN_BASE = 'https://pkgs.fluxer.com';

interface ParsedLinuxDownloadUrl {
	downloadBase: string;
	channel: DesktopDownloadChannel;
	arch: LinuxDownloadArch;
	search: string;
}

export interface LinuxManualUpdateOptionsInput {
	downloadUrl?: string | null;
	channel?: string | null;
	arch?: string | null;
	version?: string | null;
	knownOptions?: ReadonlyArray<UpdaterDownloadOption>;
}

function normalizeLinuxDownloadArch(value: string | null | undefined): LinuxDownloadArch {
	const normalized = value?.trim().toLowerCase();
	if (normalized === 'arm64' || normalized === 'aarch64') {
		return 'arm64';
	}
	return 'x64';
}

function normalizeLinuxDownloadArchOrNull(value: string | null | undefined): LinuxDownloadArch | null {
	const normalized = value?.trim();
	if (!normalized) {
		return null;
	}
	return normalizeLinuxDownloadArch(normalized);
}

function normalizeDesktopDownloadChannel(value: string | null | undefined): DesktopDownloadChannel {
	return value?.trim().toLowerCase() === 'canary' ? 'canary' : 'stable';
}

function parseLinuxDownloadUrl(value: string | null | undefined): ParsedLinuxDownloadUrl | null {
	if (!value) {
		return null;
	}
	try {
		const parsed = new URL(value);
		const segments = parsed.pathname.split('/').filter(Boolean);
		const start = segments.findIndex((segment, index) => segment === 'desktop' && segments[index + 2] === 'linux');
		if (start < 0) {
			return null;
		}
		const channel = normalizeDesktopDownloadChannel(segments[start + 1]);
		const arch = normalizeLinuxDownloadArch(segments[start + 3]);
		const version = segments[start + 4];
		const format = segments[start + 5];
		if (!version || !format) {
			return null;
		}
		const prefix = segments.slice(0, start);
		const prefixPath = prefix.length > 0 ? `/${prefix.join('/')}` : '';
		return {
			downloadBase: `${parsed.origin}${prefixPath}`,
			channel,
			arch,
			search: parsed.search,
		};
	} catch {
		return null;
	}
}

function getLinuxDownloadArchFromText(value: string | null | undefined): LinuxDownloadArch | null {
	const normalized = value?.trim().toLowerCase();
	if (!normalized) {
		return null;
	}
	if (/(^|[-_/(\s])(?:arm64|aarch64)(?=$|[-_/).\s])/u.test(normalized)) {
		return 'arm64';
	}
	if (/(^|[-_/(\s])(?:x64|x86_64|amd64)(?=$|[-_/).\s])/u.test(normalized)) {
		return 'x64';
	}
	return null;
}

function getLinuxDownloadOptionArch(option: UpdaterDownloadOption): LinuxDownloadArch | null {
	return (
		parseLinuxDownloadUrl(option.url)?.arch ??
		getLinuxDownloadArchFromText(option.suggestedName) ??
		getLinuxDownloadArchFromText(option.label)
	);
}

function getKnownLinuxManualOption(
	options: ReadonlyArray<UpdaterDownloadOption>,
	format: LinuxManualDownloadFormat,
	arch: LinuxDownloadArch,
): UpdaterDownloadOption | null {
	const formatOptions = options.filter((option) => option.format === format);
	return (
		formatOptions.find((option) => getLinuxDownloadOptionArch(option) === arch) ??
		(formatOptions.length === 1 && getLinuxDownloadOptionArch(formatOptions[0]) === null ? formatOptions[0] : null)
	);
}

function buildLinuxDownloadUrl(params: {
	downloadBase: string;
	channel: DesktopDownloadChannel;
	arch: LinuxDownloadArch;
	versionToken: string;
	format: LinuxManualDownloadFormat;
	search: string;
}): string {
	return `${params.downloadBase}/desktop/${params.channel}/linux/${params.arch}/${params.versionToken}/${params.format}${params.search}`;
}

function getModernProductName(channel: DesktopDownloadChannel): string {
	return channel === 'canary' ? CANARY_RELEASE_CHANNEL_NAME : PRODUCT_NAME;
}

function getSuggestedName(
	format: LinuxManualDownloadFormat,
	channel: DesktopDownloadChannel,
	arch: LinuxDownloadArch,
	versionToken: string,
): string {
	const archToken = LINUX_MANUAL_ARCH_TOKENS[format][arch];
	const extension = LINUX_MANUAL_FORMAT_EXTENSIONS[format];
	return `${getModernProductName(channel)}-${versionToken}-linux-${archToken}${extension}`;
}

export function buildLinuxManualUpdateOptions(input: LinuxManualUpdateOptionsInput): Array<UpdaterDownloadOption> {
	const parsedUrl = parseLinuxDownloadUrl(input.downloadUrl);
	const channel = parsedUrl?.channel ?? normalizeDesktopDownloadChannel(input.channel);
	const arch = normalizeLinuxDownloadArchOrNull(input.arch) ?? parsedUrl?.arch ?? 'x64';
	const downloadBase = parsedUrl?.downloadBase ?? PACKAGE_ORIGIN_BASE;
	const search = parsedUrl?.search ?? '';
	const version = input.version?.trim() ?? '';
	const hasVersion = /^\d+\.\d+\.\d+$/u.test(version);
	const versionToken = hasVersion ? version : 'latest';
	const knownOptions = hasVersion ? (input.knownOptions ?? []) : [];
	return LINUX_MANUAL_DOWNLOAD_FORMATS.map((format) => {
		const knownOption = getKnownLinuxManualOption(knownOptions, format, arch);
		return {
			format,
			label: LINUX_MANUAL_FORMAT_LABELS[format],
			url: buildLinuxDownloadUrl({downloadBase, channel, arch, versionToken, format, search}),
			suggestedName: knownOption?.suggestedName ?? getSuggestedName(format, channel, arch, versionToken),
			sha256: knownOption?.sha256 ?? null,
		};
	});
}

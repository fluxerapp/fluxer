// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {makeAttachmentCdnUrl} from '@app/api/channel/services/message/MessageHelpers';
import {extractTimestampBigInt} from '@fluxer/snowflake/src/SnowflakeUtils';
import {
	attachmentStorageKeyFromUrl,
	type SignAttachmentUrlOptions,
	signDataPackageAttachmentUrl as signDataPackageWithSecret,
	signAttachmentUrl as signWithSecret,
	stripAttachmentSignature as stripSignature,
} from '@pkgs/media_proxy_utils/src/AttachmentUrlSignature';

const SNOWFLAKE_SEGMENT_REGEX = /^[0-9]{1,20}$/u;
const STORAGE_KEY_MIN_SEGMENTS = 4;

function signingSecret(): Buffer | null {
	const configured = Config.mediaProxy.attachmentUrls.secretsBase64[0];
	if (!configured) return null;
	const secret = Buffer.from(configured, 'base64');
	return secret.length === 0 ? null : secret;
}

function anchorSecsFromStorageKey(storageKey: string): number | null {
	const segments = storageKey.split('/');
	if (segments.length < STORAGE_KEY_MIN_SEGMENTS || segments[0] !== 'attachments') return null;
	const attachmentId = segments[2] as string;
	if (!SNOWFLAKE_SEGMENT_REGEX.test(segments[1] as string) || !SNOWFLAKE_SEGMENT_REGEX.test(attachmentId)) return null;
	return Math.floor(extractTimestampBigInt(BigInt(attachmentId)) / 1000);
}

function signingOptions(url: string, nowSecs?: number): SignAttachmentUrlOptions | null {
	const secret = signingSecret();
	if (secret === null) return null;
	const mediaEndpoint = Config.endpoints.media;
	const storageKey = attachmentStorageKeyFromUrl(url, mediaEndpoint);
	if (storageKey === null) return null;
	const anchorSecs = anchorSecsFromStorageKey(storageKey);
	if (anchorSecs === null) return null;
	return {mediaEndpoint, secret, nowSecs: nowSecs ?? Math.floor(Date.now() / 1000), anchorSecs};
}

export function signAttachmentUrl(url: string, nowSecs?: number): string {
	const options = signingOptions(url, nowSecs);
	return options === null ? url : signWithSecret(url, options);
}

export function signDataPackageAttachmentUrl(url: string, nowSecs?: number): string {
	const options = signingOptions(url, nowSecs);
	return options === null ? url : signDataPackageWithSecret(url, options);
}

export function stripOwnAttachmentSignature(url: string): string {
	return attachmentStorageKeyFromUrl(url, Config.endpoints.media) === null ? url : stripSignature(url);
}

export function makeSignedAttachmentCdnUrl(
	channelId: ChannelID,
	attachmentId: AttachmentID | bigint,
	filename: string,
): string {
	return signAttachmentUrl(makeAttachmentCdnUrl(channelId, attachmentId, filename));
}

export function makeDataPackageAttachmentCdnUrl(
	channelId: ChannelID,
	attachmentId: AttachmentID | bigint,
	filename: string,
): string {
	return signDataPackageAttachmentUrl(makeAttachmentCdnUrl(channelId, attachmentId, filename));
}

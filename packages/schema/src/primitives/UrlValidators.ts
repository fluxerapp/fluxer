// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {normalizeString, withStringLengthRangeValidation} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import validator from 'validator';
import {z} from 'zod';

const PROTOCOLS = ['http', 'https'];
const FILENAME_SAFE_REGEX = /^[\p{L}\p{N}\p{M}_.-]+$/u;
const HOSTNAME_LABEL_REGEX = /^[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?$/;
const URL_VALIDATOR_OPTIONS = {
	require_protocol: true,
	require_host: true,
	disallow_auth: true,
	allow_trailing_dot: false,
	allow_protocol_relative_urls: false,
	allow_fragments: true,
	validate_length: true,
	protocols: ['http', 'https'] as Array<string>,
} as const;

let isDevelopment = false;

export function setIsDevelopment(value: boolean): void {
	isDevelopment = value;
}

function createUrlSchema() {
	return z
		.string()
		.transform(normalizeString)
		.pipe(withStringLengthRangeValidation(z.string(), 1, 2048, ValidationErrorCodes.URL_LENGTH_INVALID))
		.refine((value) => {
			if (!value.startsWith('http://') && !value.startsWith('https://')) {
				return false;
			}
			try {
				const url = new URL(value);
				return PROTOCOLS.includes(url.protocol.slice(0, -1));
			} catch {
				return false;
			}
		}, ValidationErrorCodes.INVALID_URL_FORMAT)
		.refine(
			(value) =>
				validator.isURL(value, {
					...URL_VALIDATOR_OPTIONS,
					require_tld: !isDevelopment,
				}),
			ValidationErrorCodes.INVALID_URL_FORMAT,
		);
}

export function isFqdnHostname(hostname: string): boolean {
	if (!hostname || hostname.length > 253 || !hostname.includes('.')) {
		return false;
	}
	const labels = hostname.split('.');
	for (const label of labels) {
		if (!label || label.length > 63 || !HOSTNAME_LABEL_REGEX.test(label)) {
			return false;
		}
	}
	return !/^\d+$/.test(labels[labels.length - 1]);
}

export const HostnameType = z
	.string()
	.transform((value) => {
		const trimmed = value.trim().toLowerCase();
		return trimmed.endsWith('.') ? trimmed.slice(0, -1) : trimmed;
	})
	.refine(isFqdnHostname, ValidationErrorCodes.INVALID_FORMAT);

export const URLType = createUrlSchema();
export const AttachmentURLType = z
	.string()
	.transform(normalizeString)
	.pipe(withStringLengthRangeValidation(z.string(), 1, 2048, ValidationErrorCodes.URL_LENGTH_INVALID))
	.refine((value) => {
		if (value.startsWith('attachment://')) {
			const filename = value.slice(13);
			if (filename.length === 0) {
				return false;
			}
			return FILENAME_SAFE_REGEX.test(filename);
		}
		if (!value.startsWith('http://') && !value.startsWith('https://')) {
			return false;
		}
		try {
			const url = new URL(value);
			return PROTOCOLS.includes(url.protocol.slice(0, -1));
		} catch {
			return false;
		}
	}, ValidationErrorCodes.INVALID_URL_OR_ATTACHMENT_FORMAT)
	.refine((value) => {
		if (value.startsWith('attachment://')) {
			return true;
		}
		return validator.isURL(value, {
			...URL_VALIDATOR_OPTIONS,
			require_tld: !isDevelopment,
		});
	}, ValidationErrorCodes.INVALID_URL_FORMAT);

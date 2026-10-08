// SPDX-License-Identifier: AGPL-3.0-or-later

import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {
	ASSET_FORMAT_POLICY,
	type AssetKind,
	getAcceptString as getPolicyAcceptString,
} from '@fluxer/constants/src/AssetFormatPolicy';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export {getAcceptString} from '@fluxer/constants/src/AssetFormatPolicy';

const UNSUPPORTED_FILE_TYPE_ALLOWED_FORMATS_DESCRIPTOR = msg({
	message: 'Unsupported file type.',
	comment: 'Form validation error for an asset of an unsupported file type.',
});
const FILE_IS_TOO_LARGE_MAXIMUM_SIZE_IS_DESCRIPTOR = msg({
	message: 'File is too large. Maximum size is {maxSize}.',
	comment: 'Form validation error for an asset that exceeds the maximum file size.',
});
const IMAGE_DIMENSIONS_ARE_OUT_OF_RANGE_DESCRIPTOR = msg({
	message: 'Image dimensions are out of range.',
	comment: 'Form validation error for an asset image with invalid dimensions.',
});
const IMAGE_DIMENSIONS_MUST_BE_BETWEEN_X_AND_X_DESCRIPTOR = msg({
	message: 'Image dimensions must be between {min}x{min2} and {max}x{max2} pixels.',
	comment: 'Form validation error describing valid asset image dimensions.',
});
const ANIMATED_UPLOADS_REQUIRE_A_PREMIUM_SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'Animated uploads require a {premiumProductName} subscription.',
	comment:
		'Validation error shown when uploading an animated asset without the required subscription. Preserve {premiumProductName}; it is inserted by code.',
});
export function getAcceptStringFiltered(kind: AssetKind, animatedAllowed: boolean): string {
	return getPolicyAcceptString(kind, {animatedAllowed});
}

export type AssetCopyErrorReason = 'unsupported_mime' | 'too_large' | 'bad_dimensions' | 'animated_requires_premium';

function formatBytes(bytes: number): string {
	if (bytes >= 1024 * 1024) {
		const mb = bytes / (1024 * 1024);
		const rounded = Math.round(mb * 10) / 10;
		return `${rounded % 1 === 0 ? rounded.toFixed(0) : rounded.toFixed(1)} MB`;
	}
	const kb = Math.round(bytes / 1024);
	return `${kb} KB`;
}

export function getAssetFormatErrorMessage(i18n: I18n, kind: AssetKind, reason: AssetCopyErrorReason): string {
	const policy = ASSET_FORMAT_POLICY[kind];
	switch (reason) {
		case 'unsupported_mime': {
			return i18n._(UNSUPPORTED_FILE_TYPE_ALLOWED_FORMATS_DESCRIPTOR);
		}
		case 'too_large': {
			const maxSize = formatBytes(policy.maxBytes);
			return i18n._(FILE_IS_TOO_LARGE_MAXIMUM_SIZE_IS_DESCRIPTOR, {maxSize});
		}
		case 'bad_dimensions': {
			if (policy.dims == null) {
				return i18n._(IMAGE_DIMENSIONS_ARE_OUT_OF_RANGE_DESCRIPTOR);
			}
			const min = policy.dims.min;
			const max = policy.dims.max;
			return i18n._(IMAGE_DIMENSIONS_MUST_BE_BETWEEN_X_AND_X_DESCRIPTOR, {min, min2: min, max, max2: max});
		}
		case 'animated_requires_premium': {
			return i18n._(ANIMATED_UPLOADS_REQUIRE_A_PREMIUM_SUBSCRIPTION_DESCRIPTOR, {
				premiumProductName: getPremiumProductName(),
			});
		}
	}
}

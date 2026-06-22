// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

function sanitizeImage(image: string | undefined): string | undefined {
	if (!image) return undefined;
	if (image.startsWith('data:') || image.startsWith('blob:')) {
		return undefined;
	}
	if (image.length > 256) return undefined;
	return image;
}

export function sanitizeActivityAssetsForGateway(activity: UserActivity): UserActivity {
	if (!activity.assets) return activity;
	const largeImage = sanitizeImage(activity.assets.large_image);
	const smallImage = sanitizeImage(activity.assets.small_image);
	if (largeImage === activity.assets.large_image && smallImage === activity.assets.small_image) {
		return activity;
	}
	return {
		...activity,
		assets: {
			...activity.assets,
			large_image: largeImage,
			small_image: smallImage,
		},
	};
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveActivityImageUrl} from '@app/features/presence/utils/resolveActivityImageUrl';
import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type React from 'react';
import {useEffect, useMemo, useState} from 'react';

interface ActivityCoverImageProps {
	activity: UserActivity;
	className?: string;
	fallback: React.ReactNode;
}

function canLoadActivityImage(url: string): boolean {
	return url.startsWith('http://') || url.startsWith('https://');
}

export function ActivityCoverImage({activity, className, fallback}: ActivityCoverImageProps) {
	const [failed, setFailed] = useState(false);
	const iconUrl = useMemo(() => {
		const primaryImage = activity.assets?.large_image ?? activity.assets?.small_image;
		return resolveActivityImageUrl(primaryImage, activity.application_id);
	}, [activity.application_id, activity.assets?.large_image, activity.assets?.small_image]);
	const badgeUrl = useMemo(() => {
		if (!activity.assets?.large_image || !activity.assets.small_image) return null;
		return resolveActivityImageUrl(activity.assets.small_image, activity.application_id);
	}, [activity.application_id, activity.assets?.large_image, activity.assets?.small_image]);
	useEffect(() => {
		setFailed(false);
	}, [badgeUrl, iconUrl]);
	if (!iconUrl || failed || !canLoadActivityImage(iconUrl)) {
		return <>{fallback}</>;
	}
	return (
		<div style={{position: 'relative'}} data-flx="presence.activity-cover-image">
			<img
				className={className}
				src={iconUrl}
				alt=""
				title={activity.assets?.large_text ?? activity.assets?.small_text}
				referrerPolicy="no-referrer"
				decoding="async"
				onError={() => setFailed(true)}
			/>
			{badgeUrl && canLoadActivityImage(badgeUrl) ? (
				<img
					className={className}
					data-rpc-small-image-badge="true"
					src={badgeUrl}
					alt=""
					title={activity.assets?.small_text}
					referrerPolicy="no-referrer"
					decoding="async"
				/>
			) : null}
		</div>
	);
}

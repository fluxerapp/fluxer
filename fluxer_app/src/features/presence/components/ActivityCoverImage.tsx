// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveActivityImageUrl} from '@app/features/presence/utils/resolveActivityImageUrl';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type React from 'react';
import {useEffect, useMemo, useState} from 'react';

interface ActivityCoverImageProps {
	activity: UserActivity;
	className?: string;
	fallback: React.ReactNode;
}

function canLoadActivityImage(url: string): boolean {
	if (url.startsWith('http://') || url.startsWith('https://')) return true;
	if (url.startsWith('fluxer-rpc-art://')) return isDesktop();
	return true;
}

export function ActivityCoverImage({activity, className, fallback}: ActivityCoverImageProps) {
	const [failed, setFailed] = useState(false);
	const iconUrl = useMemo(
		() => resolveActivityImageUrl(activity.assets?.large_image, activity.application_id),
		[activity.application_id, activity.assets?.large_image],
	);
	useEffect(() => {
		setFailed(false);
	}, [iconUrl]);
	if (!iconUrl || failed || !canLoadActivityImage(iconUrl)) {
		return <>{fallback}</>;
	}
	return (
		<img
			className={className}
			src={iconUrl}
			alt=""
			referrerPolicy="no-referrer"
			decoding="async"
			onError={() => setFailed(true)}
			data-flx="presence.activity-cover-image"
		/>
	);
}

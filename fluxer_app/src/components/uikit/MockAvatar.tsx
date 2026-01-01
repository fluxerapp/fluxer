/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import React from 'react';
import {getStatusTypeLabel} from '~/Constants';
import {BaseAvatar} from '~/components/uikit/BaseAvatar';
import {cdnUrl} from '~/utils/UrlUtils';

interface MockAvatarProps {
	size: 12 | 16 | 20 | 24 | 32 | 36 | 40 | 48 | 56 | 80 | 120;
	avatarUrl?: string;
	hoverAvatarUrl?: string;
	status?: string | null;
	isTyping?: boolean;
	showOffline?: boolean;
	className?: string;
	isClickable?: boolean;
	userTag?: string;
	disableStatusTooltip?: boolean;
	shouldPlayAnimated?: boolean;
	isMobileStatus?: boolean;
}

export const MockAvatar = React.forwardRef<HTMLDivElement, MockAvatarProps>(
	(
		{
			size,
			avatarUrl = cdnUrl('avatars/0.png'),
			hoverAvatarUrl,
			status,
			isTyping = false,
			showOffline = true,
			className,
			isClickable = false,
			userTag = 'Mock User',
			disableStatusTooltip = false,
			shouldPlayAnimated = false,
			isMobileStatus = false,
			...props
		},
		ref,
	) => {
		const {i18n} = useLingui();
		const statusLabel = status != null ? getStatusTypeLabel(i18n, status) : null;

		return (
			<BaseAvatar
				ref={ref}
				size={size}
				avatarUrl={avatarUrl}
				hoverAvatarUrl={hoverAvatarUrl}
				status={status}
				shouldPlayAnimated={shouldPlayAnimated}
				isTyping={isTyping}
				showOffline={showOffline}
				className={className}
				isClickable={isClickable}
				userTag={userTag}
				statusLabel={statusLabel}
				disableStatusTooltip={disableStatusTooltip}
				isMobileStatus={isMobileStatus}
				{...props}
			/>
		);
	},
);

MockAvatar.displayName = 'MockAvatar';

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IconProps} from '@phosphor-icons/react';
import React from 'react';

export const AnnouncementChannelIcon = React.forwardRef<SVGSVGElement, IconProps>(
	({size = 256, className, ...props}, ref) => (
		<svg
			ref={ref}
			width={size}
			height={size}
			viewBox="0 0 256 256"
			fill="none"
			xmlns="http://www.w3.org/2000/svg"
			className={className}
			aria-hidden={true}
			data-flx="ui.icons.announcement-channel-icon.svg"
			{...props}
		>
			<path
				fill="currentColor"
				stroke="currentColor"
				strokeLinecap="round"
				strokeLinejoin="round"
				strokeWidth={24}
				d="M36 96H96L212 40V216L96 160H36ZM68 160L84 212"
				data-flx="ui.icons.announcement-channel-icon.path"
			/>
		</svg>
	),
);

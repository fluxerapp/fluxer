// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IconProps} from '@phosphor-icons/react';
import React from 'react';

export const ForumChannelIcon = React.forwardRef<SVGSVGElement, IconProps>(({size = 256, className, ...props}, ref) => {
	return (
		<svg
			ref={ref}
			width={size}
			height={size}
			viewBox="0 0 256 256"
			fill="none"
			xmlns="http://www.w3.org/2000/svg"
			className={className}
			aria-hidden={true}
			data-flx="ui.icons.locked-announcement-channel-icon.svg"
			{...props}
		>
			<path
				fill="currentColor"
				stroke="currentColor"
				strokeLinecap="round"
				strokeLinejoin="round"
				d="M169.57,72.59A80,80,0,0,0,16,104v64a16,16,0,0,0,16,16H86.67A80.15,80.15,0,0,0,160,232h64a16,16,0,0,0,16-16V152A80,80,0,0,0,169.57,72.59ZM224,216H160a64.14,64.14,0,0,1-55.68-32.43A79.93,79.93,0,0,0,174.7,89.71,64,64,0,0,1,224,152Z"
				data-flx="ui.icons.locked-announcement-channel-icon.path"
			/>
		</svg>
	);
});

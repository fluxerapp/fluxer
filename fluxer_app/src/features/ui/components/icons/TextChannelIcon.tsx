// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IconProps} from '@phosphor-icons/react';
import React from 'react';

export const TextChannelIcon = React.forwardRef<SVGSVGElement, IconProps>(({size = 256, className, ...props}, ref) => (
	<svg
		ref={ref}
		width={size}
		height={size}
		viewBox="0 0 256 256"
		fill="none"
		xmlns="http://www.w3.org/2000/svg"
		className={className}
		aria-hidden={true}
		data-flx="ui.icons.text-channel-icon.svg"
		{...props}
	>
		<path
			fill="currentColor"
			fillRule="evenodd"
			d="M164.193 37.8536c1.186-6.5205 7.433-10.8456 13.953-9.6602 6.521 1.1856 10.846 7.4327 9.661 13.9531L180.197 84H224c6.627 0 12 5.3726 12 12 0 6.627-5.373 12-12 12h-48.167l-7.272 40H208c6.627 0 12 5.373 12 12s-5.373 12-12 12h-43.803l-8.39 46.147c-1.186 6.52-7.433 10.845-13.953 9.66-6.521-1.186-10.846-7.433-9.661-13.953l7.61-41.854h-39.606l-8.3904 46.147c-1.1856 6.52-7.4327 10.845-13.9531 9.66-6.5204-1.186-10.8455-7.433-9.6601-13.953L75.8027 172H32c-6.6274 0-11.9999-5.373-12-12 0-6.627 5.3726-12 12-12h48.167l7.2725-40H48c-6.6274 0-11.9999-5.373-12-12 0-6.6274 5.3726-12 12-12h43.8027l8.3903-46.1464c1.186-6.5205 7.433-10.8456 13.953-9.6602 6.521 1.1856 10.846 7.4327 9.661 13.9531L116.197 84h39.606zM104.561 148h39.606l7.272-40h-39.606z"
			clipRule="evenodd"
			data-flx="ui.icons.text-channel-icon.path"
		/>
	</svg>
));

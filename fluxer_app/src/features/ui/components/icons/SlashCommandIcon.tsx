// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import type {IconProps} from '@phosphor-icons/react';
import React from 'react';

export const SlashCommandIcon = React.forwardRef<SVGSVGElement, IconProps>(({size = 24, className, ...props}, ref) => (
	<svg
		ref={ref}
		width={typeof size === 'number' ? remFromPx(size) : size}
		height={typeof size === 'number' ? remFromPx(size) : size}
		viewBox="0 0 24 24"
		fill="none"
		xmlns="http://www.w3.org/2000/svg"
		className={className}
		aria-hidden={true}
		{...props}
	>
		<path
			fillRule="evenodd"
			clipRule="evenodd"
			d="M7 4h10a3 3 0 0 1 3 3v10a3 3 0 0 1-3 3H7a3 3 0 0 1-3-3V7a3 3 0 0 1 3-3ZM14.05 6.75h1.9L9.95 17.25h-1.9z"
			fill="currentColor"
		/>
	</svg>
));

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

import type {IconProps} from '@phosphor-icons/react';
import React from 'react';

export const NSFWIcon = React.forwardRef<SVGSVGElement, IconProps>(({size = 16, className, ...props}, ref) => (
	<svg
		ref={ref}
		width={size}
		height={size}
		viewBox="0 0 256 256"
		fill="none"
		xmlns="http://www.w3.org/2000/svg"
		className={className}
		aria-hidden={true}
		{...props}
	>
		<g clipPath="url(#clip0_83_2)">
			<path
				d="M48 96h53.5M152.5 162.5l-8.498 53.5M112 40L80 216M32 160h176"
				stroke="currentColor"
				strokeWidth={24}
				strokeLinecap="round"
				strokeLinejoin="round"
			/>
			<g clipPath="url(#clip1_83_2)">
				<path
					d="M234.4 110.045L190.675 34.11a12.377 12.377 0 00-16.825-4.475 12.387 12.387 0 00-4.525 4.475L125.6 110.045a11.757 11.757 0 000 11.86A12.18 12.18 0 00136.275 128h87.45a12.182 12.182 0 0010.665-6.095 11.76 11.76 0 00.01-11.86zM176 68a4.001 4.001 0 018 0v20a4 4 0 11-8 0V68zm4 44a6.003 6.003 0 01-5.543-3.704 5.998 5.998 0 011.301-6.539 6.003 6.003 0 016.539-1.3A6 6 0 01180 112z"
					fill="currentColor"
				/>
			</g>
		</g>
		<defs>
			<clipPath id="clip0_83_2">
				<path fill="currentColor" d="M0 0H256V256H0z" />
			</clipPath>
			<clipPath id="clip1_83_2">
				<path fill="currentColor" transform="translate(116 16)" d="M0 0H128V128H0z" />
			</clipPath>
		</defs>
	</svg>
));

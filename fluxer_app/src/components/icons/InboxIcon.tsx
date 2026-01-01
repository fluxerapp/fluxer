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

export const InboxIcon = React.forwardRef<SVGSVGElement, IconProps>(({size = 24, className, ...props}, ref) => (
	<svg
		ref={ref}
		width={size}
		height={size}
		viewBox="0 0 24 24"
		fill="none"
		xmlns="http://www.w3.org/2000/svg"
		className={className}
		aria-hidden={true}
		{...props}
	>
		<path
			d="M19.3333 2H4.15583C2.95333 2 2.01083 2.96417 2.01083 4.16667L2 19.3333C2 20.525 2.95333 21.5 4.15583 21.5H19.3333C20.525 21.5 21.5 20.525 21.5 19.3333V4.16667C21.5 2.96417 20.525 2 19.3333 2ZM19.3333 15H15C15 16.7983 13.5375 18.25 11.75 18.25C9.9625 18.25 8.5 16.7983 8.5 15H4.15583V4.16667H19.3333V15Z"
			fill="currentColor"
		/>
	</svg>
));

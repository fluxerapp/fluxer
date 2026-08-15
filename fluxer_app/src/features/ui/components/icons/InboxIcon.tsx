// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import type {IconProps} from '@phosphor-icons/react';
import React from 'react';

export const InboxIcon = React.forwardRef<SVGSVGElement, IconProps>(({size = 24, className, ...props}, ref) => {
	const dimension = typeof size === 'number' ? remFromPx(size) : size;
	return (
		<svg
			ref={ref}
			width={dimension}
			height={dimension}
			viewBox="0 0 24 24"
			fill="none"
			xmlns="http://www.w3.org/2000/svg"
			className={className}
			aria-hidden={true}
			data-flx="ui.icons.inbox-icon.svg"
			{...props}
		>
			<path
				d="M19.3333 2H4.15583C2.95333 2 2.01083 2.96417 2.01083 4.16667L2 19.3333C2 20.525 2.95333 21.5 4.15583 21.5H19.3333C20.525 21.5 21.5 20.525 21.5 19.3333V4.16667C21.5 2.96417 20.525 2 19.3333 2ZM19.3333 15H15C15 16.7983 13.5375 18.25 11.75 18.25C9.9625 18.25 8.5 16.7983 8.5 15H4.15583V4.16667H19.3333V15Z"
				fill="currentColor"
				data-flx="ui.icons.inbox-icon.path"
			/>
		</svg>
	);
});

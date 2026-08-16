// SPDX-License-Identifier: AGPL-3.0-or-later

import {useId} from 'react';

interface TraktIconProps {
	size?: number;
	className?: string;
}

export function TraktIcon({size = 16, className}: TraktIconProps) {
	const gradientId = `trakt-icon-gradient-${useId().replace(/:/g, '')}`;
	return (
		<svg
			xmlns="http://www.w3.org/2000/svg"
			width={size}
			height={size}
			viewBox="0 0 48 48"
			className={className}
			data-flx="ui.icons.trakt-icon.svg"
		>
			<defs>
				<radialGradient
					id={gradientId}
					cx="48.46"
					cy="-.95"
					r="64.84"
					fx="48.46"
					fy="-.95"
					gradientUnits="userSpaceOnUse"
				>
					<stop offset="0" stopColor="#9f42c6" />
					<stop offset=".27" stopColor="#a041c3" />
					<stop offset=".42" stopColor="#a43ebb" />
					<stop offset=".53" stopColor="#aa39ad" />
					<stop offset=".64" stopColor="#b4339a" />
					<stop offset=".73" stopColor="#c02b81" />
					<stop offset=".82" stopColor="#cf2061" />
					<stop offset=".9" stopColor="#e1143c" />
					<stop offset=".97" stopColor="#f50613" />
					<stop offset="1" stopColor="red" />
				</radialGradient>
			</defs>
			<path
				d="M48 11.26v25.47C48 42.95 42.95 48 36.73 48H11.26C5.04 48 0 42.95 0 36.73V11.26C0 5.04 5.04 0 11.26 0h25.47a11.24 11.24 0 0 1 9.62 5.4c.18.29.34.59.5.89.33.68.6 1.39.79 2.14.1.37.18.76.23 1.15.09.54.13 1.11.13 1.68Z"
				fill={`url(#${gradientId})`}
				data-flx="ui.icons.trakt-icon.background-path"
			/>
			<path
				d="m13.62 17.97 7.92 7.92 1.47-1.47-7.92-7.92-1.47 1.47Zm14.39 14.4 1.47-1.46-2.16-2.16L47.64 8.43c-.19-.75-.46-1.46-.79-2.14L24.39 28.75l3.62 3.62Zm-15.09-13.7-1.46 1.46 14.4 14.4 1.46-1.47L23 28.75 46.35 5.4c-.36-.6-.78-1.16-1.25-1.68L21.54 27.28l-8.62-8.61Zm34.95-9.09L28.7 28.75l1.47 1.46L48 12.38v-1.12c0-.57-.04-1.14-.13-1.68ZM25.16 22.27l-7.92-7.92-1.47 1.47 7.92 7.92 1.47-1.47Zm16.16 12.85c0 3.42-2.78 6.2-6.2 6.2H12.88c-3.42 0-6.2-2.78-6.2-6.2V12.88c0-3.42 2.78-6.21 6.2-6.21h20.78V4.6H12.88c-4.56 0-8.28 3.71-8.28 8.28v22.24c0 4.56 3.71 8.28 8.28 8.28h22.24c4.56 0 8.28-3.71 8.28-8.28v-3.51h-2.07v3.51Z"
				fill="#fff"
				data-flx="ui.icons.trakt-icon.path"
			/>
		</svg>
	);
}

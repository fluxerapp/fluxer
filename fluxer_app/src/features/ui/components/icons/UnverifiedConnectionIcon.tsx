// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';

interface UnverifiedConnectionIconProps {
	size?: number;
	className?: string;
}

export function UnverifiedConnectionIcon({size = 16, className}: UnverifiedConnectionIconProps) {
	return (
		<svg
			xmlns="http://www.w3.org/2000/svg"
			width={remFromPx(size)}
			height={remFromPx(size)}
			viewBox="0 0 256 256"
			fill="none"
			className={className}
			data-flx="ui.icons.unverified-connection-icon.svg"
		>
			<path
				d="M240 128C240 138.44 232.49 146.27 225.86 153.18C222.09 157.12 218.19 161.18 216.72 164.75C215.36 168.02 215.28 173.44 215.2 178.69C215.05 188.45 214.89 199.51 207.2 207.2C199.51 214.89 188.45 215.05 178.69 215.2C173.44 215.28 168.02 215.36 164.75 216.72C161.18 218.19 157.12 222.09 153.18 225.86C146.27 232.49 138.44 240 128 240C117.56 240 109.73 232.49 102.82 225.86C98.88 222.09 94.82 218.19 91.25 216.72C87.98 215.36 82.56 215.28 77.31 215.2C67.55 215.05 56.49 214.89 48.8 207.2C41.11 199.51 40.95 188.45 40.8 178.69C40.72 173.44 40.64 168.02 39.28 164.75C37.81 161.18 33.91 157.12 30.14 153.18C23.51 146.27 16 138.44 16 128C16 117.56 23.51 109.73 30.14 102.82C33.91 98.88 37.81 94.82 39.28 91.25C40.64 87.98 40.72 82.56 40.8 77.31C40.95 67.55 41.11 56.49 48.8 48.8C56.49 41.11 67.55 40.95 77.31 40.8C82.56 40.72 87.98 40.64 91.25 39.28C94.82 37.81 98.88 33.91 102.82 30.14C109.73 23.51 117.56 16 128 16C138.44 16 146.27 23.51 153.18 30.14C157.12 33.91 161.18 37.81 164.75 39.28C168.02 40.64 173.44 40.72 178.69 40.8C188.45 40.95 199.51 41.11 207.2 48.8C214.89 56.49 215.05 67.55 215.2 77.31C215.28 82.56 215.36 87.98 216.72 91.25C218.19 94.82 222.09 98.88 225.86 102.82C232.49 109.73 240 117.56 240 128Z"
				fill="#DD2E44"
				data-flx="ui.icons.unverified-connection-icon.path"
			/>
			<path
				d="M164 92L92 164"
				stroke="white"
				strokeWidth="24"
				strokeLinecap="round"
				strokeLinejoin="round"
				data-flx="ui.icons.unverified-connection-icon.path--2"
			/>
			<path
				d="M164 164L92 92"
				stroke="white"
				strokeWidth="24"
				strokeLinecap="round"
				strokeLinejoin="round"
				data-flx="ui.icons.unverified-connection-icon.path--3"
			/>
		</svg>
	);
}

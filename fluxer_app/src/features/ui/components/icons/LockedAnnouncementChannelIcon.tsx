// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IconProps} from '@phosphor-icons/react';
import React, {useId} from 'react';

export const LockedAnnouncementChannelIcon = React.forwardRef<SVGSVGElement, IconProps>(
	({size = 256, className, ...props}, ref) => {
		const maskId = useId();
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
				<g mask={`url(#${maskId})`} data-flx="ui.icons.locked-announcement-channel-icon.g">
					<path
						fill="currentColor"
						stroke="currentColor"
						strokeLinecap="round"
						strokeLinejoin="round"
						strokeWidth={24}
						d="M36 96H96L212 40V216L96 160H36ZM68 160L84 212"
						data-flx="ui.icons.locked-announcement-channel-icon.path"
					/>
				</g>
				<path
					fill="currentColor"
					d="M234.25 50.25h-12.5v-9.375c0-4.9728-1.975-9.7419-5.492-13.2583-3.516-3.5163-8.285-5.4917-13.258-5.4917s-9.742 1.9754-13.258 5.4917c-3.517 3.5164-5.492 8.2855-5.492 13.2583v9.375h-12.5c-1.658 0-3.247.6585-4.419 1.8306-1.173 1.1721-1.831 2.7618-1.831 4.4194v43.75c0 1.658.658 3.247 1.831 4.419 1.172 1.173 2.761 1.831 4.419 1.831h62.5c1.658 0 3.247-.658 4.419-1.831 1.173-1.172 1.831-2.761 1.831-4.419V56.5c0-1.6576-.658-3.2473-1.831-4.4194-1.172-1.1721-2.761-1.8306-4.419-1.8306M203 83.0625c-.927 0-1.833-.2749-2.604-.79s-1.372-1.2472-1.727-2.1037c-.354-.8565-.447-1.799-.266-2.7083.18-.9093.627-1.7445 1.282-2.4001.656-.6555 1.491-1.102 2.401-1.2828.909-.1809 1.851-.0881 2.708.2667.856.3548 1.588.9556 2.104 1.7265.515.7708.79 1.6771.79 2.6042 0 1.2432-.494 2.4355-1.373 3.3146-.88.879-2.072 1.3729-3.315 1.3729M215.5 50.25h-25v-9.375c0-3.3152 1.317-6.4946 3.661-8.8388s5.524-3.6612 8.839-3.6612 6.495 1.317 8.839 3.6612 3.661 5.5236 3.661 8.8388z"
					data-flx="ui.icons.locked-announcement-channel-icon.path--2"
				/>
				<defs data-flx="ui.icons.locked-announcement-channel-icon.defs">
					<mask id={maskId} data-flx="ui.icons.locked-announcement-channel-icon.mask">
						<path fill="white" d="M0 0h256v256H0z" data-flx="ui.icons.locked-announcement-channel-icon.path--3" />
						<path fill="black" d="M148 0h108v124H148z" data-flx="ui.icons.locked-announcement-channel-icon.path--4" />
					</mask>
				</defs>
			</svg>
		);
	},
);

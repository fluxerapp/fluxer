// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IconProps} from '@phosphor-icons/react';
import React, {useId} from 'react';

export const MatureAnnouncementChannelIcon = React.forwardRef<SVGSVGElement, IconProps>(
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
				data-flx="ui.icons.mature-announcement-channel-icon.svg"
				{...props}
			>
				<g mask={`url(#${maskId})`} data-flx="ui.icons.mature-announcement-channel-icon.g">
					<path
						fill="currentColor"
						stroke="currentColor"
						strokeLinecap="round"
						strokeLinejoin="round"
						strokeWidth={24}
						d="M36 96H96L212 40V216L96 160H36ZM68 160L84 212"
						data-flx="ui.icons.mature-announcement-channel-icon.path"
					/>
				</g>
				<path
					fill="currentColor"
					d="m245.254 92.0979-34.16-59.3242c-.854-1.4534-2.072-2.6585-3.535-3.4959C206.096 28.4405 204.44 28 202.754 28c-1.685 0-3.342.4405-4.805 1.2778-1.462.8374-2.681 2.0425-3.535 3.4959l-34.16 59.3242c-.821 1.4058-1.254 3.0047-1.254 4.6328 0 1.6282.433 3.227 1.254 4.6333.843 1.462 2.059 2.673 3.525 3.51s3.127 1.269 4.815 1.251h68.32c1.687.016 3.347-.416 4.811-1.253s2.679-2.047 3.521-3.508c.823-1.4056 1.257-3.0041 1.258-4.6322.002-1.6282-.43-3.2274-1.25-4.6339m-45.625-32.8476c0-.8288.329-1.6237.915-2.2097.587-.5861 1.381-.9153 2.21-.9153s1.624.3292 2.21.9153c.586.586.915 1.3809.915 2.2097v15.625c0 .8288-.329 1.6236-.915 2.2097-.586.586-1.381.9153-2.21.9153s-1.623-.3293-2.21-.9153c-.586-.5861-.915-1.3809-.915-2.2097zm3.125 34.375c-.927 0-1.833-.275-2.604-.79-.771-.5151-1.372-1.2472-1.726-2.1037-.355-.8565-.448-1.799-.267-2.7083s.627-1.7445 1.283-2.4001c.655-.6556 1.49-1.102 2.4-1.2829.909-.1808 1.851-.088 2.708.2668s1.589.9556 2.104 1.7264c.515.7709.79 1.6772.79 2.6043 0 1.2432-.494 2.4354-1.373 3.3145s-2.072 1.373-3.315 1.373"
					data-flx="ui.icons.mature-announcement-channel-icon.path--2"
				/>
				<defs data-flx="ui.icons.mature-announcement-channel-icon.defs">
					<mask id={maskId} data-flx="ui.icons.mature-announcement-channel-icon.mask">
						<path fill="white" d="M0 0h256v256H0z" data-flx="ui.icons.mature-announcement-channel-icon.path--3" />
						<path fill="black" d="M148 0h108v124H148z" data-flx="ui.icons.mature-announcement-channel-icon.path--4" />
					</mask>
				</defs>
			</svg>
		);
	},
);

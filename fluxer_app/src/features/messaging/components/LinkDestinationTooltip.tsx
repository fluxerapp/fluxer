// SPDX-License-Identifier: AGPL-3.0-or-later

import linkRendererStyles from '@app/features/theme/styles/LinkRenderer.module.css';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {WarningIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface LinkDestinationTooltipProps {
	url: string;
	untrusted: boolean;
	children: React.ReactElement;
}

export const LinkDestinationTooltip = observer(function LinkDestinationTooltip({
	url,
	untrusted,
	children,
}: LinkDestinationTooltipProps) {
	const text = MobileLayout.enabled
		? url
		: () => (
				<span className={linkRendererStyles.destination} data-flx="messaging.markdown.renderers.link-renderer.span">
					{untrusted && (
						<WarningIcon
							size={16}
							weight="fill"
							className={linkRendererStyles.destinationWarningIcon}
							data-flx="messaging.markdown.renderers.link-renderer.warning-icon"
						/>
					)}
					<span data-flx="messaging.markdown.renderers.link-renderer.span--2">{url}</span>
				</span>
			);
	return (
		<Tooltip
			text={text}
			type="normal"
			position="bottom"
			maxWidth="xl"
			data-flx="messaging.markdown.renderers.link-renderer.tooltip.normal"
		>
			{children}
		</Tooltip>
	);
});

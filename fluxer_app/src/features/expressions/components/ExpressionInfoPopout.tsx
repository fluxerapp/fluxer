// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	EXPRESSION_TOOLTIP_DELAY_MS,
	EXPRESSION_TOOLTIP_NUDGE_PX,
} from '@app/features/expressions/utils/ExpressionPreviewConstants';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const CLICKABLE_ANCESTOR_SELECTOR = 'a, button, [role="button"], [role="link"]';

interface ExpressionInfoPopoutProps {
	children: React.ReactElement;
	canOpenCard?: boolean;
	renderTooltip: () => React.ReactNode;
	renderCard: (props: {onClose: () => void}) => React.ReactNode;
}

export const ExpressionInfoPopout = observer(function ExpressionInfoPopout({
	children,
	canOpenCard = true,
	renderTooltip,
	renderCard,
}: ExpressionInfoPopoutProps): React.ReactElement {
	const shouldOpenOnClick = useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			if (!canOpenCard) {
				return false;
			}
			if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) {
				return false;
			}
			return event.currentTarget.parentElement?.closest(CLICKABLE_ANCESTOR_SELECTOR) == null;
		},
		[canOpenCard],
	);
	return (
		<Popout
			position="right-start"
			animationType="profile-slide"
			constrainHeight={false}
			freezePosition
			keepOpenOnTargetUnmount
			tooltip={renderTooltip}
			tooltipPosition="top"
			tooltipDelay={EXPRESSION_TOOLTIP_DELAY_MS}
			tooltipNudge={EXPRESSION_TOOLTIP_NUDGE_PX}
			render={renderCard}
			shouldOpenOnClick={shouldOpenOnClick}
			data-flx="expressions.expression-info-popout.popout"
		>
			<FocusRing offset={-2} data-flx="expressions.expression-info-popout.focus-ring">
				{children}
			</FocusRing>
		</Popout>
	);
});

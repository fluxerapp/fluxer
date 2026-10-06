// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/expressions/components/ExpressionHoverTooltipContent.module.css';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

export interface ExpressionHoverTooltipContentProps {
	displayName: string;
	previewUrl?: string | null;
}

export const ExpressionHoverTooltipContent = observer(function ExpressionHoverTooltipContent({
	displayName,
	previewUrl,
}: ExpressionHoverTooltipContentProps) {
	return (
		<div className={styles.container} data-flx="expressions.expression-hover-tooltip-content.container">
			{previewUrl && (
				<img
					src={previewUrl}
					alt=""
					draggable={false}
					className={styles.preview}
					data-flx="expressions.expression-hover-tooltip-content.preview"
				/>
			)}
			<div className={styles.text} data-flx="expressions.expression-hover-tooltip-content.text">
				<div className={styles.name} data-flx="expressions.expression-hover-tooltip-content.name">
					{displayName}
				</div>
				<div className={styles.hint} data-flx="expressions.expression-hover-tooltip-content.hint">
					<Trans comment="Muted second line of the emoji hover tooltip, hinting that clicking the emoji opens its info card.">
						Click to learn more
					</Trans>
				</div>
			</div>
		</div>
	);
});

ExpressionHoverTooltipContent.displayName = 'ExpressionHoverTooltipContent';

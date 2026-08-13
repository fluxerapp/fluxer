// SPDX-License-Identifier: AGPL-3.0-or-later

import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {GET_PREMIUM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import styles from '@app/features/ui/plutonium_upsell/PlutoniumUpsell.module.css';
import {Trans, useLingui} from '@lingui/react/macro';
import {CrownIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';

interface PlutoniumUpsellProps {
	children: React.ReactNode;
	className?: string;
	buttonText?: React.ReactNode;
	onButtonClick?: () => void;
	dismissible?: boolean;
	onDismiss?: () => void;
}

export const PlutoniumUpsell: React.FC<PlutoniumUpsellProps> = ({
	children,
	className,
	buttonText,
	onButtonClick,
	dismissible,
	onDismiss,
}) => {
	const showPremiumFeatures = shouldShowPremiumFeatures();
	const {i18n} = useLingui();
	if (!showPremiumFeatures) {
		return null;
	}
	return (
		<div className={clsx(styles.upsell, className)} data-flx="ui.plutonium-upsell.plutonium-upsell.upsell">
			<CrownIcon
				size={remFromPx(16)}
				weight="fill"
				className={styles.icon}
				data-flx="ui.plutonium-upsell.plutonium-upsell.icon"
			/>
			<div className={styles.content} data-flx="ui.plutonium-upsell.plutonium-upsell.content">
				<div className={styles.text} data-flx="ui.plutonium-upsell.plutonium-upsell.text">
					{children}
				</div>
				<div className={styles.actions} data-flx="ui.plutonium-upsell.plutonium-upsell.actions">
					<Button
						variant="inverted"
						superCompact={true}
						fitContent={true}
						onClick={onButtonClick ?? (() => PremiumModalCommands.open())}
						aria-label={i18n._(GET_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
						data-flx="ui.plutonium-upsell.plutonium-upsell.button"
					>
						{buttonText ?? i18n._(GET_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
					</Button>
					{dismissible && onDismiss && (
						<button
							type="button"
							className={styles.dismissLink}
							onClick={onDismiss}
							data-flx="ui.plutonium-upsell.plutonium-upsell.dismiss-link.button"
						>
							<Trans>Don't show this again</Trans>
						</button>
					)}
				</div>
			</div>
		</div>
	);
};

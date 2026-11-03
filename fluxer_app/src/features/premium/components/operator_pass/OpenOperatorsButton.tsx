// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_LABS_COMMUNITY_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {openOperatorsChannel} from '@app/features/premium/commands/OperatorPassLabsCommands';
import {showsOperatorsChannel} from '@app/features/premium/components/operator_pass/OperatorPassLabsGreetingField';
import {OPEN_OPERATORS_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {Button, type ButtonVariant} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const LABS_BANNED_DESCRIPTOR = msg({
	message: "This account or the network you're on is banned from {labsName}, so you can't join it.",
	comment:
		'Toast after the button "Open #operators" when the account, or the IP address it connects from, is banned from {labsName}, the community that has the #operators channel. Plain and neutral.',
});

const logger = new Logger('OpenOperatorsButton');

interface OpenOperatorsButtonProps {
	variant: ButtonVariant;
	label?: string;
	onOpened?: () => void;
	'data-flx': string;
}

export const OpenOperatorsButton = observer(function OpenOperatorsButton({
	variant,
	label,
	onOpened,
	'data-flx': flx,
}: OpenOperatorsButtonProps) {
	const {i18n} = useLingui();
	const [opening, setOpening] = useState(false);
	if (!showsOperatorsChannel()) return null;
	const open = async () => {
		setOpening(true);
		try {
			const result = await openOperatorsChannel();
			if (result === 'banned') {
				ToastCommands.error(i18n._(LABS_BANNED_DESCRIPTOR, {labsName: PRODUCT_LABS_COMMUNITY_NAME}));
				return;
			}
			onOpened?.();
			ModalCommands.popByModalType('user-settings');
		} catch (error) {
			logger.error('Failed to open #operators', error);
			ToastCommands.error(i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR));
		} finally {
			setOpening(false);
		}
	};
	return (
		<Button variant={variant} onClick={() => void open()} submitting={opening} data-flx={flx}>
			{label ?? i18n._(OPEN_OPERATORS_DESCRIPTOR)}
		</Button>
	);
});

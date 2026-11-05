// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import * as OperatorPassModalCommands from '@app/features/premium/commands/OperatorPassModalCommands';
import {isOperatorPassAvailable} from '@app/features/premium/utils/OperatorPassUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {useEffect, useRef} from 'react';

const logger = new Logger('OperatorPassRoutePage');

export function OperatorPassRoutePage() {
	const location = useLocation();
	const handledRef = useRef(false);
	useEffect(() => {
		if (handledRef.current) return;
		handledRef.current = true;
		const params = new URLSearchParams(location.search);
		RouterUtils.replaceWith(Routes.ME);
		if (!isOperatorPassAvailable()) return;
		const view = params.get('view');
		if (view === 'passes' || view === 'visionary') {
			void OperatorPassCommands.fetchPasses()
				.catch((error) => logger.warn('Failed to load Operator Passes before opening settings', error))
				.finally(() =>
					ModalCommands.push(
						ModalCommands.modal(
							() => (
								<UserSettingsModal
									initialTab="operator_pass"
									data-flx="premium.operator-pass-route-page.user-settings-modal"
								/>
							),
							'user-settings',
						),
					),
				);
			return;
		}
		OperatorPassModalCommands.openCheckoutModal({
			canceled: params.get('status') === 'cancel',
			sessionId: params.get('session_id') ?? undefined,
		});
	}, [location.search]);
	return null;
}

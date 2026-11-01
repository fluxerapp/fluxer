// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {OAuthAuthorizeFlowFooter} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthAuthorizeActions';
import {
	AUTHORIZE_APPLICATION_DESCRIPTOR,
	OAuthAuthorizeFlowPanel,
} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthAuthorizeFlowPanel';
import {useAuthorizeFlow} from '@app/features/auth/components/pages/oauth_authorize_page/state/useAuthorizeFlow';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface OAuthAuthorizeModalProps {
	search: string;
	'data-flx'?: string;
}

export const OAuthAuthorizeModal: React.FC<OAuthAuthorizeModalProps> = observer(({search}) => {
	const {i18n} = useLingui();
	const flow = useAuthorizeFlow({
		search,
		includeAccountStep: false,
		onCancel: ModalCommands.pop,
	});
	return (
		<Modal.Root size="small" centered onClose={ModalCommands.pop} data-flx="auth.oauth-authorize-modal.modal-root">
			<Modal.Header
				title={i18n._(AUTHORIZE_APPLICATION_DESCRIPTOR)}
				onClose={ModalCommands.pop}
				data-flx="auth.oauth-authorize-modal.modal-header"
			/>
			<Modal.Content data-flx="auth.oauth-authorize-modal.modal-content">
				<Modal.ContentLayout data-flx="auth.oauth-authorize-modal.modal-content-layout">
					<OAuthAuthorizeFlowPanel
						flow={flow}
						onDone={ModalCommands.pop}
						showInlineActions={false}
						data-flx="auth.o-auth-authorize-modal.o-auth-authorize-flow-panel"
					/>
				</Modal.ContentLayout>
			</Modal.Content>
			{flow.phase.kind === 'review' && flow.phase.step !== 'account' && (
				<Modal.Footer data-flx="auth.oauth-authorize-modal.modal-footer">
					<OAuthAuthorizeFlowFooter flow={flow} data-flx="auth.o-auth-authorize-modal.o-auth-authorize-flow-footer" />
				</Modal.Footer>
			)}
		</Modal.Root>
	);
});

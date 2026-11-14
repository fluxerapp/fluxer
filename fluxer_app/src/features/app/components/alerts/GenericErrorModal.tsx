// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {observer} from 'mobx-react-lite';

interface GenericErrorModalProps {
	title: string;
	message: React.ReactNode;
	'data-flx'?: string;
}

export const GenericErrorModal: React.FC<GenericErrorModalProps> = observer(({title, message, 'data-flx': dataFlx}) => (
	<ConfirmModal title={title} description={message} data-flx={dataFlx ?? 'app.generic-error-modal.confirm-modal'} />
));

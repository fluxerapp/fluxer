// SPDX-License-Identifier: AGPL-3.0-or-later

import MemberList from '@app/features/member/state/MemberList';
import {Logger} from '@app/features/platform/utils/AppLogger';
import MobileLayout from '@app/features/ui/state/MobileLayout';

const logger = new Logger('Layout');

export function updateMobileLayoutState(navExpanded: boolean, chatExpanded: boolean): void {
	logger.debug(`Updating mobile layout state: nav=${navExpanded}, chat=${chatExpanded}`);
	MobileLayout.updateState({navExpanded, chatExpanded});
}

export function toggleMembers(_isOpen: boolean): void {
	MemberList.toggleMembers();
}

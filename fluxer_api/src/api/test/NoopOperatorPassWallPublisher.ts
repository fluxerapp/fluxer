// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IOperatorPassWallPublisher} from '@app/api/operator_pass/OperatorPassWallPublisher';

export class NoopOperatorPassWallPublisher implements IOperatorPassWallPublisher {
	async publish(): Promise<void> {}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

export interface LoggerInterface {
	trace(msg: string): void;
	trace(obj: object, msg?: string): void;
	debug(msg: string): void;
	debug(obj: object, msg?: string): void;
	info(msg: string): void;
	info(obj: object, msg?: string): void;
	warn(msg: string): void;
	warn(obj: object, msg?: string): void;
	error(msg: string): void;
	error(obj: object, msg?: string): void;
	fatal(msg: string): void;
	fatal(obj: object, msg?: string): void;
	child(bindings: Record<string, unknown>): LoggerInterface;
}

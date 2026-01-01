/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

package commands

import (
	"fmt"
	"os"
	"os/exec"
	"strings"

	"github.com/spf13/cobra"

	"fluxer.dev/dev/pkg/integrations"
	"fluxer.dev/dev/pkg/utils"
)

const (
	defaultComposeFile = "dev/compose.yaml"
	defaultEnvFile     = "dev/.env"
)

// NewRootCmd creates the root command
func NewRootCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "devctl",
		Short: "Fluxer development control tool",
		Long:  "Docker Compose wrapper and development utilities for Fluxer.",
	}

	cmd.AddCommand(
		NewUpCmd(),
		NewDownCmd(),
		NewRestartCmd(),
		NewLogsCmd(),
		NewPsCmd(),
		NewExecCmd(),
		NewShellCmd(),

		NewLivekitSyncCmd(),
		NewGeoIPDownloadCmd(),
		NewEnsureNetworkCmd(),
	)

	return cmd
}

// NewUpCmd starts services
func NewUpCmd() *cobra.Command {
	var detach bool
	var build bool

	cmd := &cobra.Command{
		Use:   "up [services...]",
		Short: "Start services",
		Long:  "Start all or specific services using docker compose",
		RunE: func(cmd *cobra.Command, services []string) error {
			args := []string{"--env-file", defaultEnvFile, "-f", defaultComposeFile, "up"}
			if detach {
				args = append(args, "-d")
			}
			if build {
				args = append(args, "--build")
			}
			args = append(args, services...)
			return runDockerCompose(args...)
		},
	}

	cmd.Flags().BoolVarP(&detach, "detach", "d", true, "Run in background")
	cmd.Flags().BoolVar(&build, "build", false, "Build images before starting")

	return cmd
}

// NewDownCmd stops and removes containers
func NewDownCmd() *cobra.Command {
	var volumes bool

	cmd := &cobra.Command{
		Use:   "down",
		Short: "Stop and remove containers",
		RunE: func(cmd *cobra.Command, args []string) error {
			dcArgs := []string{"--env-file", defaultEnvFile, "-f", defaultComposeFile, "down"}
			if volumes {
				dcArgs = append(dcArgs, "-v")
			}
			return runDockerCompose(dcArgs...)
		},
	}

	cmd.Flags().BoolVarP(&volumes, "volumes", "v", false, "Remove volumes")

	return cmd
}

// NewRestartCmd restarts services
func NewRestartCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "restart [services...]",
		Short: "Restart services",
		RunE: func(cmd *cobra.Command, services []string) error {
			args := []string{"--env-file", defaultEnvFile, "-f", defaultComposeFile, "restart"}
			args = append(args, services...)
			return runDockerCompose(args...)
		},
	}

	return cmd
}

// NewLogsCmd shows service logs
func NewLogsCmd() *cobra.Command {
	var follow bool
	var tail string

	cmd := &cobra.Command{
		Use:   "logs [services...]",
		Short: "Show service logs",
		RunE: func(cmd *cobra.Command, services []string) error {
			args := []string{"--env-file", defaultEnvFile, "-f", defaultComposeFile, "logs"}
			if follow {
				args = append(args, "-f")
			}
			if tail != "" {
				args = append(args, "--tail", tail)
			}
			args = append(args, services...)
			return runDockerCompose(args...)
		},
	}

	cmd.Flags().BoolVarP(&follow, "follow", "f", true, "Follow log output")
	cmd.Flags().StringVarP(&tail, "tail", "n", "100", "Number of lines to show from the end")

	return cmd
}

// NewPsCmd lists containers
func NewPsCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "ps",
		Short: "List containers",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runDockerCompose("--env-file", defaultEnvFile, "-f", defaultComposeFile, "ps")
		},
	}

	return cmd
}

// NewExecCmd executes a command in a running container
func NewExecCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "exec SERVICE COMMAND...",
		Short: "Execute a command in a running container",
		Args:  cobra.MinimumNArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			dcArgs := []string{"--env-file", defaultEnvFile, "-f", defaultComposeFile, "exec"}
			dcArgs = append(dcArgs, args...)
			return runDockerCompose(dcArgs...)
		},
	}

	return cmd
}

// NewShellCmd opens a shell in a container
func NewShellCmd() *cobra.Command {
	var shell string

	cmd := &cobra.Command{
		Use:   "sh SERVICE",
		Short: "Open a shell in a container",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			service := args[0]
			return runDockerCompose("--env-file", defaultEnvFile, "-f", defaultComposeFile, "exec", service, shell)
		},
	}

	cmd.Flags().StringVar(&shell, "shell", "sh", "Shell to use (sh, bash, etc.)")

	return cmd
}

// NewLivekitSyncCmd syncs LiveKit configuration
func NewLivekitSyncCmd() *cobra.Command {
	var envPath string
	var outputPath string

	cmd := &cobra.Command{
		Use:   "livekit-sync",
		Short: "Generate LiveKit configuration from environment variables",
		RunE: func(cmd *cobra.Command, args []string) error {
			env, err := utils.ParseEnvFile(envPath)
			if err != nil {
				return fmt.Errorf("failed to read env file: %w", err)
			}

			written, err := integrations.WriteLivekitFileFromEnv(outputPath, env)
			if err != nil {
				return err
			}

			if !written {
				fmt.Println("⚠️  Voice/LiveKit is disabled - no config generated")
				return nil
			}

			fmt.Printf("✅ LiveKit config written to %s\n", outputPath)
			return nil
		},
	}

	cmd.Flags().StringVarP(&envPath, "env", "e", defaultEnvFile, "Environment file path")
	cmd.Flags().StringVarP(&outputPath, "output", "o", "dev/livekit.yaml", "Output path")

	return cmd
}

// NewGeoIPDownloadCmd downloads GeoIP database
func NewGeoIPDownloadCmd() *cobra.Command {
	var token string
	var envPath string

	cmd := &cobra.Command{
		Use:   "geoip-download",
		Short: "Download GeoIP database from IPInfo",
		RunE: func(cmd *cobra.Command, args []string) error {
			return integrations.DownloadGeoIP(token, envPath)
		},
	}

	cmd.Flags().StringVar(&token, "token", "", "IPInfo API token")
	cmd.Flags().StringVarP(&envPath, "env", "e", defaultEnvFile, "Env file to read token from")

	return cmd
}

// NewEnsureNetworkCmd ensures the Docker network exists
func NewEnsureNetworkCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "ensure-network",
		Short: "Ensure the fluxer-shared Docker network exists",
		RunE: func(cmd *cobra.Command, args []string) error {
			return ensureNetwork()
		},
	}

	return cmd
}

// runDockerCompose runs a docker compose command
func runDockerCompose(args ...string) error {
	cmd := exec.Command("docker", append([]string{"compose"}, args...)...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin
	return cmd.Run()
}

// ensureNetwork ensures the fluxer-shared network exists
func ensureNetwork() error {
	checkCmd := exec.Command("docker", "network", "ls", "--format", "{{.Name}}")
	output, err := checkCmd.Output()
	if err != nil {
		return fmt.Errorf("failed to list networks: %w", err)
	}

	networks := strings.Split(strings.TrimSpace(string(output)), "\n")
	for _, net := range networks {
		if net == "fluxer-shared" {
			fmt.Println("✅ fluxer-shared network already exists")
			return nil
		}
	}

	fmt.Println("Creating fluxer-shared network...")
	createCmd := exec.Command("docker", "network", "create", "fluxer-shared")
	createCmd.Stdout = os.Stdout
	createCmd.Stderr = os.Stderr
	if err := createCmd.Run(); err != nil {
		return fmt.Errorf("failed to create network: %w", err)
	}

	fmt.Println("✅ fluxer-shared network created")
	return nil
}

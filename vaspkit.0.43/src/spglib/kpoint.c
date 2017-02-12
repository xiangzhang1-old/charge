/* kpoint.c */
/* Copyright (C) 2008 Atsushi Togo */

#include <stdio.h>
#include <stdlib.h>
#include "mathfunc.h"
#include "kpoint.h"

#include "debug.h"

static int search_space[][3] = {
  {0, 0, 0},
  {0, 0, 1},
  {0, 1, -1},
  {0, 1, 0},
  {0, 1, 1},
  {1, -1, -1},
  {1, -1, 0},
  {1, -1, 1},
  {1, 0, -1},
  {1, 0, 0},
  {1, 0, 1},
  {1, 1, -1},
  {1, 1, 0},
  {1, 1, 1},
  {-1, -1, -1},
  {-1, -1, 0},
  {-1, -1, 1},
  {-1, 0, -1},
  {-1, 0, 0},
  {-1, 0, 1},
  {-1, 1, -1},
  {-1, 1, 0},
  {-1, 1, 1},
  {0, -1, -1},
  {0, -1, 0},
  {0, -1, 1},
  {0, 0, -1}
};

static MatINT *get_point_group_reciprocal(const MatINT * rotations,
					  const int is_time_reversal);
static MatINT *get_point_group_reciprocal_with_q(const MatINT * rot_reciprocal,
						 const double symprec,
						 const int num_q,
						 SPGCONST double qpoints[][3]);
static int get_ir_reciprocal_mesh(int grid_address[][3],
				  int map[],
				  const int mesh[3],
				  const int is_shift[3],
				  const MatINT * rot_reciprocal);
static int
get_ir_reciprocal_mesh_openmp(int grid_address[][3],
			      int map[],
			      const int mesh[3],
			      const int is_shift[3],
			      const MatINT* rot_reciprocal);
static int relocate_BZ_grid_address(int bz_grid_address[][3],
				    int bz_map[],
				    SPGCONST int grid_address[][3],
				    const int mesh[3],
				    SPGCONST double rec_lattice[3][3],
				    const int is_shift[3]);
static double get_tolerance_for_BZ_reduction(SPGCONST double rec_lattice[3][3]);
static int get_ir_triplets_at_q(int map_triplets[],
				int map_q[],
				int grid_address[][3],
				const int grid_point,
				const int mesh[3],
				const MatINT * rot_reciprocal);
static int get_BZ_triplets_at_q(int triplets[][3],
				const int grid_point,
				SPGCONST int bz_grid_address[][3],
				const int bz_map[],
				const int map_triplets[],
				const int num_map_triplets,
				const int mesh[3]);
static int get_third_q_of_triplets_at_q(int address[3][3],
					const int q_index,
					const int bz_map[],
					const int mesh[3],
					const int bzmesh[3],
					const int bzmesh_double[3]);
static int get_grid_point_double_mesh(const int address_double[3],
				      const int mesh[3]);
static int get_grid_point_single_mesh(const int address[3],
				      const int mesh[3]);
static void grid_point_to_address_double(int address_double[3],
					 const int grid_point,
					 const int mesh[3],
					 const int is_shift[3]);
static void get_grid_address(int address[3],
			     const int address_double[3],
			     const int mesh[3]);
static void get_vector_modulo(int v[3], const int m[3]);

int kpt_get_grid_point(const int grid_address[3],
		       const int mesh[3])
{
  int grid_address_modulo[3];
  mat_copy_vector_i3(grid_address_modulo, grid_address);
  get_vector_modulo(grid_address_modulo, mesh);
  return get_grid_point_single_mesh(grid_address_modulo, mesh);
}

/* grid_address (e.g. 4x4x4 mesh, unless GRID_ORDER_XYZ is defined) */
/*    [[ 0  0  0]                                                   */
/*     [ 1  0  0]                                                   */
/*     [ 2  0  0]                                                   */
/*     [-1  0  0]                                                   */
/*     [ 0  1  0]                                                   */
/*     [ 1  1  0]                                                   */
/*     [ 2  1  0]                                                   */
/*     [-1  1  0]                                                   */
/*     ....      ]                                                  */
/*                                                                  */
/* Each value of 'map' correspnds to the index of grid_point.       */
int kpt_get_irreducible_reciprocal_mesh(int grid_address[][3],
					int map[],
					const int mesh[3],
					const int is_shift[3],
					const int is_time_reversal,
					const MatINT *rotations)
{
  int num_ir;
  MatINT *rot_reciprocal;

  rot_reciprocal = get_point_group_reciprocal(rotations, is_time_reversal);

#ifdef _OPENMP
  num_ir = get_ir_reciprocal_mesh_openmp(grid_address,
					 map,
					 mesh,
					 is_shift,
					 rot_reciprocal);
  
#else
  num_ir = get_ir_reciprocal_mesh(grid_address,
				  map,
				  mesh,
				  is_shift,
				  rot_reciprocal);
#endif
  
  mat_free_MatINT(rot_reciprocal);
  return num_ir;
}

int kpt_get_stabilized_reciprocal_mesh(int grid_address[][3],
				       int map[],
				       const int mesh[3],
				       const int is_shift[3],
				       const int is_time_reversal,
				       const MatINT * rotations,
				       const int num_q,
				       SPGCONST double qpoints[][3])
{
  int num_ir;
  MatINT *rot_reciprocal, *rot_reciprocal_q;
  double tolerance;
  
  rot_reciprocal = get_point_group_reciprocal(rotations, is_time_reversal);
  tolerance = 0.01 / (mesh[0] + mesh[1] + mesh[2]);
  rot_reciprocal_q = get_point_group_reciprocal_with_q(rot_reciprocal,
						       tolerance,
						       num_q,
						       qpoints);

#ifdef _OPENMP
  num_ir = get_ir_reciprocal_mesh_openmp(grid_address,
					 map,
					 mesh,
					 is_shift,
					 rot_reciprocal_q);
#else
  num_ir = get_ir_reciprocal_mesh(grid_address,
				  map,
				  mesh,
				  is_shift,
				  rot_reciprocal_q);
#endif
  mat_free_MatINT(rot_reciprocal_q);
  mat_free_MatINT(rot_reciprocal);
  return num_ir;
}

void kpt_get_grid_points_by_rotations(int rot_grid_points[],
				      const int address_orig[3],
				      const MatINT * rot_reciprocal,
				      const int mesh[3],
				      const int is_shift[3])
{
  int i;
  int address_double_orig[3], address_double[3], mesh_double[3];

  for (i = 0; i < 3; i++) {
    mesh_double[i] = mesh[i] * 2;
    address_double_orig[i] = address_orig[i] * 2 + is_shift[i];
  }
  for (i = 0; i < rot_reciprocal->size; i++) {
    mat_multiply_matrix_vector_i3(address_double,
				  rot_reciprocal->mat[i],
				  address_double_orig);
    get_vector_modulo(address_double, mesh_double);
    rot_grid_points[i] = get_grid_point_double_mesh(address_double, mesh);
  }
}

void kpt_get_BZ_grid_points_by_rotations(int rot_grid_points[],
					 const int address_orig[3],
					 const MatINT * rot_reciprocal,
					 const int mesh[3],
					 const int is_shift[3],
					 const int bz_map[])
{
  int i;
  int address_double_orig[3], address_double[3], mesh_double[3], bzmesh_double[3];

  for (i = 0; i < 3; i++) {
    mesh_double[i] = mesh[i] * 2;
    bzmesh_double[i] = mesh[i] * 4;
    address_double_orig[i] = address_orig[i] * 2 + is_shift[i];
  }
  for (i = 0; i < rot_reciprocal->size; i++) {
    mat_multiply_matrix_vector_i3(address_double,
				  rot_reciprocal->mat[i],
				  address_double_orig);
    get_vector_modulo(address_double, bzmesh_double);
    rot_grid_points[i] =
      bz_map[get_grid_point_double_mesh(address_double, mesh_double)];
  }
}

int kpt_relocate_BZ_grid_address(int bz_grid_address[][3],
				 int bz_map[],
				 SPGCONST int grid_address[][3],
				 const int mesh[3],
				 SPGCONST double rec_lattice[3][3],
				 const int is_shift[3])
{
  return relocate_BZ_grid_address(bz_grid_address,
				  bz_map,
				  grid_address,
				  mesh,
				  rec_lattice,
				  is_shift);
}

int kpt_get_ir_triplets_at_q(int map_triplets[],
			     int map_q[],
			     int grid_address[][3],
			     const int grid_point,
			     const int mesh[3],
			     const int is_time_reversal,
			     const MatINT * rotations)
{
  int num_ir;
  MatINT *rot_reciprocal;

  rot_reciprocal = get_point_group_reciprocal(rotations, is_time_reversal);
  num_ir = get_ir_triplets_at_q(map_triplets,
				map_q,
				grid_address,
				grid_point,
				mesh,
				rot_reciprocal);
  mat_free_MatINT(rot_reciprocal);
  return num_ir;
}

int kpt_get_BZ_triplets_at_q(int triplets[][3],
			     const int grid_point,
			     SPGCONST int bz_grid_address[][3],
			     const int bz_map[],
			     const int map_triplets[],
			     const int num_map_triplets,
			     const int mesh[3])
{
  return get_BZ_triplets_at_q(triplets,
			      grid_point,
			      bz_grid_address,
			      bz_map,
			      map_triplets,
			      num_map_triplets,
			      mesh);
}

void kpt_get_neighboring_grid_points(int neighboring_grid_points[],
				     const int grid_point,
				     SPGCONST int relative_grid_address[][3],
				     const int num_relative_grid_address,
				     const int mesh[3],
				     SPGCONST int bz_grid_address[][3],
				     const int bz_map[])
{
  int mesh_double[3], bzmesh[3], bzmesh_double[3],
    address_double[3], bz_address_double[3];
  int i, j, bz_gp;

  for (i = 0; i < 3; i++) {
    mesh_double[i] = mesh[i] * 2;
    bzmesh[i] = mesh[i] * 2;
    bzmesh_double[i] = bzmesh[i] * 2;
  }
  for (i = 0; i < num_relative_grid_address; i++) {
    for (j = 0; j < 3; j++) {
      address_double[j] = (bz_grid_address[grid_point][j] +
			   relative_grid_address[i][j]) * 2;
      bz_address_double[j] = address_double[j];
    }
    get_vector_modulo(bz_address_double, bzmesh_double);
    bz_gp = bz_map[get_grid_point_double_mesh(bz_address_double, bzmesh)];
    if (bz_gp == -1) {
      get_vector_modulo(address_double, mesh_double);
      neighboring_grid_points[i] =
	get_grid_point_double_mesh(address_double, mesh);
    } else {
      neighboring_grid_points[i] = bz_gp;
    }
  }
}

static MatINT *get_point_group_reciprocal(const MatINT * rotations,
					  const int is_time_reversal)
{
  int i, j, num_rot;
  MatINT *rot_reciprocal, *rot_return;
  int *unique_rot;
  SPGCONST int inversion[3][3] = {
    {-1, 0, 0 },
    { 0,-1, 0 },
    { 0, 0,-1 }
  };
  
  if (is_time_reversal) {
    rot_reciprocal = mat_alloc_MatINT(rotations->size * 2);
  } else {
    rot_reciprocal = mat_alloc_MatINT(rotations->size);
  }
  unique_rot = (int*)malloc(sizeof(int) * rot_reciprocal->size);
  for (i = 0; i < rot_reciprocal->size; i++) {
    unique_rot[i] = -1;
  }

  for (i = 0; i < rotations->size; i++) {
    mat_transpose_matrix_i3(rot_reciprocal->mat[i], rotations->mat[i]);
    
    if (is_time_reversal) {
      mat_multiply_matrix_i3(rot_reciprocal->mat[rotations->size+i],
			     inversion,
			     rot_reciprocal->mat[i]);
    }
  }

  num_rot = 0;
  for (i = 0; i < rot_reciprocal->size; i++) {
    for (j = 0; j < num_rot; j++) {
      if (mat_check_identity_matrix_i3(rot_reciprocal->mat[unique_rot[j]],
				       rot_reciprocal->mat[i])) {
	goto escape;
      }
    }
    unique_rot[num_rot] = i;
    num_rot++;
  escape:
    ;
  }

  rot_return = mat_alloc_MatINT(num_rot);
  for (i = 0; i < num_rot; i++) {
    mat_copy_matrix_i3(rot_return->mat[i], rot_reciprocal->mat[unique_rot[i]]);    }
  free(unique_rot);
  mat_free_MatINT(rot_reciprocal);

  return rot_return;
}

static MatINT *get_point_group_reciprocal_with_q(const MatINT * rot_reciprocal,
						 const double symprec,
						 const int num_q,
						 SPGCONST double qpoints[][3])
{
  int i, j, k, l, is_all_ok, num_rot;
  int *ir_rot;
  double q_rot[3], diff[3];
  MatINT * rot_reciprocal_q;

  is_all_ok = 0;
  num_rot = 0;
  ir_rot = (int*)malloc(sizeof(int) * rot_reciprocal->size);
  for (i = 0; i < rot_reciprocal->size; i++) {
    ir_rot[i] = -1;
  }
  for (i = 0; i < rot_reciprocal->size; i++) {
    for (j = 0; j < num_q; j++) {
      is_all_ok = 0;
      mat_multiply_matrix_vector_id3(q_rot,
				     rot_reciprocal->mat[i],
				     qpoints[j]);

      for (k = 0; k < num_q; k++) {
	for (l = 0; l < 3; l++) {
	  diff[l] = q_rot[l] - qpoints[k][l];
	  diff[l] -= mat_Nint(diff[l]);
	}
	
	if (mat_Dabs(diff[0]) < symprec && 
	    mat_Dabs(diff[1]) < symprec &&
	    mat_Dabs(diff[2]) < symprec) {
	  is_all_ok = 1;
	  break;
	}
      }
      
      if (! is_all_ok) {
	break;
      }
    }

    if (is_all_ok) {
      ir_rot[num_rot] = i;
      num_rot++;
    }
  }

  rot_reciprocal_q = mat_alloc_MatINT(num_rot);
  for (i = 0; i < num_rot; i++) {
    mat_copy_matrix_i3(rot_reciprocal_q->mat[i],
		       rot_reciprocal->mat[ir_rot[i]]);  
  }

  free(ir_rot);

  return rot_reciprocal_q;
}

static int get_ir_reciprocal_mesh(int grid_address[][3],
				  int map[],
				  const int mesh[3],
				  const int is_shift[3],
				  const MatINT *rot_reciprocal)
{
  /* In the following loop, mesh is doubled. */
  /* Even and odd mesh numbers correspond to */
  /* is_shift[i] are 0 or 1, respectively. */
  /* is_shift = [0,0,0] gives Gamma center mesh. */
  /* grid: reducible grid points */
  /* map: the mapping from each point to ir-point. */

  int i, j, k, l, grid_point, grid_point_rot, num_ir = 0;
  int address_double[3], address_rot[3], mesh_double[3];

  for (i = 0; i < 3; i++) {
    mesh_double[i] = mesh[i] * 2;
  }

  /* "-1" means the element is not touched yet. */
  for (i = 0; i < mesh[0] * mesh[1] * mesh[2]; i++) {
    map[i] = -1;
  }

#ifndef GRID_ORDER_XYZ
  for (i = 0; i < mesh[2]; i++) {
    for (j = 0; j < mesh[1]; j++) {
      for (k = 0; k < mesh[0]; k++) {
	address_double[0] = k * 2 + is_shift[0];
	address_double[1] = j * 2 + is_shift[1];
	address_double[2] = i * 2 + is_shift[2];
#else
  for (i = 0; i < mesh[0]; i++) {
    for (j = 0; j < mesh[1]; j++) {
      for (k = 0; k < mesh[2]; k++) {
  	address_double[0] = i * 2 + is_shift[0];
  	address_double[1] = j * 2 + is_shift[1];
  	address_double[2] = k * 2 + is_shift[2];
#endif	

	grid_point = get_grid_point_double_mesh(address_double, mesh);
	get_grid_address(grid_address[grid_point], address_double, mesh);

	for (l = 0; l < rot_reciprocal->size; l++) {
	  mat_multiply_matrix_vector_i3(address_rot,
					rot_reciprocal->mat[l],
					address_double);
	  get_vector_modulo(address_rot, mesh_double);
	  grid_point_rot = get_grid_point_double_mesh(address_rot, mesh);

	  if (grid_point_rot > -1) { /* Invalid if even --> odd or odd --> even */
	    if (map[grid_point_rot] > -1) {
	      map[grid_point] = map[grid_point_rot];
	      break;
	    }
	  }
	}
	
	if (map[grid_point] == -1) {
	  map[grid_point] = grid_point;
	  num_ir++;
	}
      }
    }
  }

  return num_ir;
}

static int
get_ir_reciprocal_mesh_openmp(int grid_address[][3],
			      int map[],
			      const int mesh[3],
			      const int is_shift[3],
			      const MatINT * rot_reciprocal)
{
  int i, j, k, l, grid_point, grid_point_rot, num_ir;
  int address_double[3], address_rot[3], mesh_double[3];

  for (i = 0; i < 3; i++) {
    mesh_double[i] = mesh[i] * 2;
  }

#ifndef GRID_ORDER_XYZ
#pragma omp parallel for private(j, k, l, grid_point, grid_point_rot, address_double, address_rot)
  for (i = 0; i < mesh[2]; i++) {
    for (j = 0; j < mesh[1]; j++) {
      for (k = 0; k < mesh[0]; k++) {
	address_double[0] = k * 2 + is_shift[0];
	address_double[1] = j * 2 + is_shift[1];
	address_double[2] = i * 2 + is_shift[2];
#else
#pragma omp parallel for private(j, k, l, grid_point, grid_point_rot, address_double, address_rot)
  for (i = 0; i < mesh[0]; i++) {
    for (j = 0; j < mesh[1]; j++) {
      for (k = 0; k < mesh[2]; k++) {
  	address_double[0] = i * 2 + is_shift[0];
  	address_double[1] = j * 2 + is_shift[1];
  	address_double[2] = k * 2 + is_shift[2];
#endif	

	grid_point = get_grid_point_double_mesh(address_double, mesh);
	map[grid_point] = grid_point;
	get_grid_address(grid_address[grid_point], address_double, mesh);

	for (l = 0; l < rot_reciprocal->size; l++) {
	  mat_multiply_matrix_vector_i3(address_rot,
					rot_reciprocal->mat[l],
					address_double);
	  get_vector_modulo(address_rot, mesh_double);
	  grid_point_rot = get_grid_point_double_mesh(address_rot, mesh);

	  if (grid_point_rot > -1) { /* Invalid if even --> odd or odd --> even */
	    if (grid_point_rot < map[grid_point]) {
	      map[grid_point] = grid_point_rot;
	    }
	  }
	}
      }
    }
  }

  num_ir = 0;

#pragma omp parallel for reduction(+:num_ir)
  for (i = 0; i < mesh[0] * mesh[1] * mesh[2]; i++) {
    if (map[i] == i) {
      num_ir++;
    }
  }
  
  return num_ir;
}

/* Relocate grid addresses to first Brillouin zone */
/* bz_grid_address[prod(mesh + 1)][3] */
/* bz_map[prod(mesh * 2)] */
static int relocate_BZ_grid_address(int bz_grid_address[][3],
				    int bz_map[],
				    SPGCONST int grid_address[][3],
				    const int mesh[3],
				    SPGCONST double rec_lattice[3][3],
				    const int is_shift[3])
{
  double tolerance, min_distance;
  double vector[3], distance[27];
  int bzmesh[3], bzmesh_double[3], address_double[3];
  int i, j, k, min_index, boundary_num_gp, total_num_gp, bzgp, gp;

  tolerance = get_tolerance_for_BZ_reduction(rec_lattice);
  for (i = 0; i < 3; i++) {
    bzmesh[i] = mesh[i] * 2;
    bzmesh_double[i] = bzmesh[i] * 2;
  }
  for (i = 0; i < bzmesh[0] * bzmesh[1] * bzmesh[2]; i++) {
    bz_map[i] = -1;
  }
  
  boundary_num_gp = 0;
  total_num_gp = mesh[0] * mesh[1] * mesh[2];
  for (i = 0; i < total_num_gp; i++) {
    for (j = 0; j < 27; j++) {
      for (k = 0; k < 3; k++) {
	address_double[k] =
	  (grid_address[i][k] + search_space[j][k] * mesh[k]) * 2 + is_shift[k];
      }
      mat_multiply_matrix_vector_di3(vector, rec_lattice, address_double);
      distance[j] = mat_norm_squared_d3(vector);
    }
    min_distance = distance[0];
    min_index = 0;
    for (j = 1; j < 27; j++) {
      if (distance[j] + tolerance < min_distance) {
	min_distance = distance[j];
	min_index = j;
      }
    }

    for (j = 0; j < 27; j++) {
      if (distance[j] < min_distance + tolerance) {
	if (j == min_index) {
	  gp = i;
	} else {
	  gp = boundary_num_gp + total_num_gp;
	}
	for (k = 0; k < 3; k++) {
	  bz_grid_address[gp][k] = 
	    grid_address[i][k] + search_space[j][k] * mesh[k];
	  address_double[k] = bz_grid_address[gp][k] * 2 + is_shift[k];
	  if (address_double[k] < 0) {
	    address_double[k] += bzmesh_double[k];
	  }
	}
	bzgp = get_grid_point_double_mesh(address_double, bzmesh);
	bz_map[bzgp] = gp;
	if (j != min_index) {
	  boundary_num_gp++;
	}
      }
    }
  }

  return boundary_num_gp + total_num_gp;
}

static double get_tolerance_for_BZ_reduction(SPGCONST double rec_lattice[3][3])
{
  int i, j;
  double tolerance;
  double length[3];
  
  for (i = 0; i < 3; i++) {
    length[i] = 0;
    for (j = 0; j < 3; j++) {
      length[i] += rec_lattice[j][i] * rec_lattice[j][i];
    }
  }
  tolerance = length[0];
  for (i = 1; i < 3; i++) {
    if (tolerance > length[i]) {
      tolerance = length[i];
    }
  }
  tolerance *= 0.01;
  return tolerance;
}
 
static int get_ir_triplets_at_q(int map_triplets[],
				int map_q[],
				int grid_address[][3],
				const int grid_point,
				const int mesh[3],
				const MatINT * rot_reciprocal)
{
  int i, j, num_grid, q_2, num_ir_q, num_ir_triplets, ir_grid_point;
  int mesh_double[3], is_shift[3];
  int address_double0[3], address_double1[3], address_double2[3];
  int *ir_grid_points, *third_q;
  double tolerance;
  double stabilizer_q[1][3];
  MatINT *rot_reciprocal_q;

  tolerance = 0.01 / (mesh[0] + mesh[1] + mesh[2]);
  num_grid = mesh[0] * mesh[1] * mesh[2];

  for (i = 0; i < 3; i++) {
    /* Only consider the gamma-point */
    is_shift[i] = 0;
    mesh_double[i] = mesh[i] * 2;
  }

  /* Search irreducible q-points (map_q) with a stabilizer */
  /* q */  
  grid_point_to_address_double(address_double0, grid_point, mesh, is_shift);
  for (i = 0; i < 3; i++) {
    stabilizer_q[0][i] =
      (double)address_double0[i] / mesh_double[i] - (address_double0[i] > mesh[i]);
  }

  rot_reciprocal_q = get_point_group_reciprocal_with_q(rot_reciprocal,
						       tolerance,
						       1,
						       stabilizer_q);
#ifdef _OPENMP
  num_ir_q = get_ir_reciprocal_mesh_openmp(grid_address,
					   map_q,
					   mesh,
					   is_shift,
					   rot_reciprocal_q);
#else
  num_ir_q = get_ir_reciprocal_mesh(grid_address,
				    map_q,
				    mesh,
				    is_shift,
				    rot_reciprocal_q);
#endif
  mat_free_MatINT(rot_reciprocal_q);

  third_q = (int*) malloc(sizeof(int) * num_ir_q);
  ir_grid_points = (int*) malloc(sizeof(int) * num_ir_q);
  num_ir_q = 0;
  for (i = 0; i < num_grid; i++) {
    if (map_q[i] == i) {
      ir_grid_points[num_ir_q] = i;
      num_ir_q++;
    }
    map_triplets[i] = -1;
  }

#pragma omp parallel for private(j, address_double1, address_double2)
  for (i = 0; i < num_ir_q; i++) {
    grid_point_to_address_double(address_double1,
				 ir_grid_points[i],
				 mesh,
				 is_shift); /* q' */
    for (j = 0; j < 3; j++) { /* q'' */
      address_double2[j] = - address_double0[j] - address_double1[j];
    }
    get_vector_modulo(address_double2, mesh_double);
    third_q[i] = get_grid_point_double_mesh(address_double2, mesh);
  }

  num_ir_triplets = 0;
  for (i = 0; i < num_ir_q; i++) {
    ir_grid_point = ir_grid_points[i];
    q_2 = third_q[i];
    if (map_triplets[map_q[q_2]] > -1) {
      map_triplets[ir_grid_point] = map_q[q_2];
    } else {
      map_triplets[ir_grid_point] = ir_grid_point;
      num_ir_triplets++;
    }
  }

#pragma omp parallel for
  for (i = 0; i < num_grid; i++) {
    map_triplets[i] = map_triplets[map_q[i]];
  }
  
  free(third_q);
  third_q = NULL;
  free(ir_grid_points);
  ir_grid_points = NULL;

  return num_ir_triplets;
}

static int get_BZ_triplets_at_q(int triplets[][3],
				const int grid_point,
				SPGCONST int bz_grid_address[][3],
				const int bz_map[],
				const int map_triplets[],
				const int num_map_triplets,
				const int mesh[3])
{
  int i, j, k, num_ir;
  int address[3][3], address_double[3], bzmesh[3], bzmesh_double[3];
  int *ir_grid_points;

  for (i = 0; i < 3; i++) {
    bzmesh[i] = mesh[i] * 2;
    bzmesh_double[i] = bzmesh[i] * 2;
  }

  num_ir = 0;
  ir_grid_points = (int*) malloc(sizeof(int) * num_map_triplets);
  for (i = 0; i < num_map_triplets; i++) {
    if (map_triplets[i] == i) {
      ir_grid_points[num_ir] = i;
      num_ir++;
    }
  }
 
#pragma omp parallel for private(j, k, address, address_double)
  for (i = 0; i < num_ir; i++) {
    for (j = 0; j < 3; j++) {
      address[0][j] = bz_grid_address[grid_point][j];
      address[1][j] = bz_grid_address[ir_grid_points[i]][j];
      address[2][j] = - address[0][j] - address[1][j];
    }
    for (j = 2; j > -1; j--) {
      if (get_third_q_of_triplets_at_q(address,
    				       j,
    				       bz_map,
    				       mesh,
    				       bzmesh,
    				       bzmesh_double) == 0) {
    	break;
      }
    }
    for (j = 0; j < 3; j++) {
      for (k = 0; k < 3; k++) {
	address_double[k] = address[j][k] * 2;
	if (address_double[k] < 0) {
	  address_double[k] += bzmesh_double[k];
	}
      }
      triplets[i][j] =
	bz_map[get_grid_point_double_mesh(address_double, bzmesh)];
    }
  }

  free(ir_grid_points);
  
  return num_ir;
}

static int get_third_q_of_triplets_at_q(int address[3][3],
					const int q_index,
					const int bz_map[],
					const int mesh[3],
					const int bzmesh[3],
					const int bzmesh_double[3])
{
  int i, j, smallest_g, smallest_index, sum_g, delta_g[3];
  int bzgp[27], address_double[3];

  get_vector_modulo(address[q_index], mesh);
  for (i = 0; i < 3; i++) {
    delta_g[i] = 0;
    for (j = 0; j < 3; j++) {
      delta_g[i] += address[j][i];
    }
    delta_g[i] /= mesh[i];
  }
  
  for (i = 0; i < 27; i++) {
    for (j = 0; j < 3; j++) {
      address_double[j] = (address[q_index][j] +
			   search_space[i][j] * mesh[j]) * 2;
    }
    for (j = 0; j < 3; j++) {
      if (address_double[j] < 0) {
	address_double[j] += bzmesh_double[j];
      }
    }
    bzgp[i] = bz_map[get_grid_point_double_mesh(address_double, bzmesh)];
  }

  for (i = 0; i < 27; i++) {
    if (bzgp[i] != -1) {
      goto escape;
    }
  }
  warning_print("******* Warning *******\n");
  warning_print(" No third-q was found.\n");
  warning_print("******* Warning *******\n");

 escape:

  smallest_g = 4;
  smallest_index = 0;

  for (i = 0; i < 27; i++) {
    if (bzgp[i] > -1) { /* q'' is in BZ */
      sum_g = (abs(delta_g[0] + search_space[i][0]) +
	       abs(delta_g[1] + search_space[i][1]) +
	       abs(delta_g[2] + search_space[i][2]));
      if (sum_g < smallest_g) {
	smallest_index = i;
	smallest_g = sum_g;
      }
    }
  }

  for (i = 0; i < 3; i++) {
    address[q_index][i] += search_space[smallest_index][i] * mesh[i];
  }

  return smallest_g;
}

static int get_grid_point_double_mesh(const int address_double[3],
				      const int mesh[3])
{
  int i, address[3];

  for (i = 0; i < 3; i++) {
    if (address_double[i] % 2 == 0) {
      address[i] = address_double[i] / 2;
    } else {
      address[i] = (address_double[i] - 1) / 2;
    }
  }
  return get_grid_point_single_mesh(address, mesh);
}

static int get_grid_point_single_mesh(const int address[3],
				      const int mesh[3])
{  
#ifndef GRID_ORDER_XYZ
  return address[2] * mesh[0] * mesh[1] + address[1] * mesh[0] + address[0];
#else
  return address[0] * mesh[1] * mesh[2] + address[1] * mesh[2] + address[2];
#endif  
}

static void grid_point_to_address_double(int address_double[3],
					 const int grid_point,
					 const int mesh[3],
					 const int is_shift[3])
{
  int i;
  int address[3];

#ifndef GRID_ORDER_XYZ
  address[2] = grid_point / (mesh[0] * mesh[1]);
  address[1] = (grid_point - address[2] * mesh[0] * mesh[1]) / mesh[0];
  address[0] = grid_point % mesh[0];
#else
  address[0] = grid_point / (mesh[1] * mesh[2]);
  address[1] = (grid_point - address[0] * mesh[1] * mesh[2]) / mesh[2];
  address[2] = grid_point % mesh[2];
#endif

  for (i = 0; i < 3; i++) {
    address_double[i] = address[i] * 2 + is_shift[i];
  }
}

static void get_grid_address(int address[3],
			     const int address_double[3],
			     const int mesh[3])
{
  int i;

  for (i = 0; i < 3; i++) {
    if (address_double[i] % 2 == 0) {
      address[i] = address_double[i] / 2;
    } else {
      address[i] = (address_double[i] - 1) / 2;
    }

#ifndef GRID_BOUNDARY_AS_NEGATIVE
    address[i] -= mesh[i] * (address[i] > mesh[i] / 2);
#else
    address[i] -= mesh[i] * (address[i] >= mesh[i] / 2);
#endif
  }  
}

static void get_vector_modulo(int v[3], const int m[3])
{
  int i;

  for (i = 0; i < 3; i++) {
    v[i] = v[i] % m[i];

    if (v[i] < 0)
      v[i] += m[i];
  }
}

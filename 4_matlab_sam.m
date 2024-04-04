%fileTimeAndPair === [file_index, time_index, target_index, source_index]
 

  load results.mat
  
  for i = 1:4
    Arr(:,i) = fileTimeAndPair(:,i);
  end
  
  Arr(:,5) = localTranEntropy(:,1);
  
  writematrix(Arr, 'local_transfer_entropy_raw.txt','Delimiter','tab')
  
  
